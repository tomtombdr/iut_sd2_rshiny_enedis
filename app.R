# app.R
# install.packages(c("ggplot2","leaflet","shiny","dplyr", "sf"))
library(ggplot2)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer) # Ajout pour les palettes de couleurs si nécessaire

# Assurez-vous que le fichier CSV est dans le bon chemin
df_total <- read.csv2(file = "DATA/données_projet_DPE.csv", stringsAsFactors = FALSE)

# --- Préparation initiale des données pour les filtres ---
# Assurer que 'code_insee_ban' est un caractère
df_total$code_insee_ban <- as.character(df_total$code_insee_ban)

# Création d'une colonne pour le code départemental (les deux premiers chiffres)
df_total$code_dept <- substr(df_total$code_insee_ban, 1, 2)

# Définition des options pour le filtre "neufancien"
neufancien_choices <- c("Les deux" = "les_deux", "Ancien" = "ancien", "Neuf" = "neuf")

# --- Définition de l'Interface Utilisateur (UI) ---
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Présentation du DPE sur les logements neufs et existants en Savoie et Haute-Savoie"),
  
  # Utilisation d'un layout avec barre latérale
  sidebarLayout(
    
    # Panneau de la barre latérale pour les filtres (commun à tous les onglets)
    sidebarPanel(
      h4("Filtres de Données"),
      
      # 1. Filtre par code départemental
      selectInput("code_postal_filtre",
                  "Filtrer par Département (Code INSEE):",
                  choices = c("Les deux" = "tous", 
                              "Savoie (73)" = "73", 
                              "Haute-Savoie (74)" = "74"),
                  selected = "tous"),
      
      # 2. Filtre par type de logement (Neuf/Ancien)
      selectInput("neufancien_filtre",
                  "Type de Logement:",
                  choices = neufancien_choices,
                  selected = "les_deux"),
      
      hr(), # Ligne de séparation
      p(em("Les graphiques et la carte ci-dessous sont mis à jour en fonction de ces filtres."))
    ),
    
    # Panneau principal pour les onglets
    mainPanel(
      # Création des onglets (tabsetPanel)
      tabsetPanel(
        
        # Onglet 1 : Comparaison territoire (Histogrammes modifiés)
        tabPanel("Comparaison territoire",
                 h3("Répartition des surfaces habitables des maisons (Départements 73 et 74)"),
                 plotOutput("Répartition_surface_maison_73"),
                 plotOutput("Répartition_surface_maison_74")),
        
        # Onglet 2 : Carte avec clustering
        tabPanel("Carte avec clustering", 
                 fluidRow(
                   column(12,
                          h3("Localisation de tous les logements (Clustering)", align = "center", style = "margin-top: 20px; color: #1a5276;"),
                          # Output pour la carte Leaflet
                          leafletOutput("dpe_map", height = "700px")
                   )
                 )
        ),
        
        # Onglet 3 : Corrélation
        tabPanel("Corrélation",
                 h3("Corrélation entre les Étiquettes Énergie et Climat"),
                 plotOutput("Correlation_ges_dpe"))
      )
    )
  )
)

server <- function(input, output) {
  
  # --- 1. Expression Réactive pour le Filtrage des Données ---
  filtered_data <- reactive({
    data <- df_total
    
    # FILTRE 1: Code Postal (Département)
    if (input$code_postal_filtre != "tous") {
      data <- data %>%
        filter(code_dept == input$code_postal_filtre)
    }
    
    # FILTRE 2: Neuf / Ancien
    if (input$neufancien_filtre != "les_deux") {
      # Assurez-vous que la colonne neufancien est nettoyée ou correspond aux valeurs du filtre
      # Le filtre utilise les valeurs brutes "ancien" et "neuf" du jeu de données
      data <- data %>%
        filter(neufancien == input$neufancien_filtre)
    }
    
    return(data)
  })
  
  
  # --- 2. Reactive expression pour préparer les données de la carte (coordonnées) ---
  map_data <- reactive({
    # Utiliser les données filtrées
    data <- filtered_data()
    
    # 1. Conversion des coordonnées X/Y en numérique pour Leaflet.
    data <- data %>%
      mutate(
        # Remplacement de la virgule par le point pour la conversion
        x_lambert = as.numeric(gsub(",", ".", coordonnee_cartographique_x_ban)),
        y_lambert = as.numeric(gsub(",", ".", coordonnee_cartographique_y_ban))
      )
    
    # Filtrer les lignes avec des coordonnées NA ou non valides après la première conversion
    data <- data %>% 
      filter(!is.na(x_lambert) & !is.na(y_lambert) & !is.nan(x_lambert) & !is.nan(y_lambert))
    
    # S'il ne reste aucune donnée valide, on retourne un data frame vide
    if(nrow(data) == 0) {
      return(data.frame(longitude = numeric(0), latitude = numeric(0), etiquette_dpe = character(0)))
    }
    
    # 2. Conversion du Lambert 93 (EPSG:2154) au WGS84 (EPSG:4326)
    # On ignore les warnings liés aux NA qui ont déjà été gérés
    suppressWarnings({
      sf_points <- data %>%
        st_as_sf(coords = c("x_lambert", "y_lambert"), crs = 2154) %>% # Définir le CRS d'origine (Lambert 93)
        st_transform(crs = 4326) # Transformer vers le CRS cible (WGS84 pour Leaflet)
      
      # Récupérer les nouvelles coordonnées Longitude/Latitude
      coords_wgs84 <- st_coordinates(sf_points)
      
      # Ajouter les coordonnées au data frame et sélectionner les colonnes
      data_wgs84 <- data.frame(
        st_drop_geometry(sf_points), # Supprimer la colonne géométrique pour ne garder que les données
        longitude = coords_wgs84[,1],
        latitude = coords_wgs84[,2]
      )
    })
    
    # 3. FILTRAGE DES POINTS ABERRANTS (Hors de France métropolitaine)
    data_filtered <- data_wgs84 %>%
      filter(longitude >= -5 & longitude <= 10 & 
               latitude >= 42 & latitude <= 51)
    
    # Retourner les données filtrées
    data_filtered %>%
      select(longitude, latitude, etiquette_dpe, surface_habitable_logement, type_batiment, everything())
  })
  
  
  # --- 3. Outputs pour les Histogrammes (Répartitions 73 et 74) ---
  output$Répartition_surface_maison_73 <- renderPlot({
    
    # Utiliser les données filtrées
    df_filtered <- filtered_data()
    
    df_maison = df_filtered[df_filtered$type_batiment == "maison",]
    # Filtrer la surface et le département 73
    df_maison_73 = df_maison[df_maison$surface_habitable_logement <= 350 & 
                               df_maison$code_dept == "73", ]
    
    # Si le filtre départemental n'inclut pas le 73, le graphique sera vide (ou si pas de données)
    if(nrow(df_maison_73) == 0 || input$code_postal_filtre == "74") {
      return(NULL) # Ne rien afficher
    }
    
    ggplot(df_maison_73, aes(x = surface_habitable_logement)) +
      geom_histogram(
        breaks = seq(0, 350, length.out = 8), 
        fill = "#5DADE2", # Nouvelle couleur pour la distinction
        color = "black"
      ) +
      scale_x_continuous(limits = c(0, 350)) +
      scale_y_continuous(limits = c(0, max(2000, max(table(cut(df_maison_73$surface_habitable_logement, breaks = seq(0, 350, length.out = 8))))*1.1, na.rm = TRUE))) +
      labs(
        title = "Répartition des surfaces habitables des maisons du 73",
        x = "Surface habitable (m²)",
        y = "Effectifs"
      ) +
      theme_light(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$Répartition_surface_maison_74 <- renderPlot({
    
    # Utiliser les données filtrées
    df_filtered <- filtered_data()
    
    df_maison = df_filtered[df_filtered$type_batiment == "maison",]
    # Filtrer la surface et le département 74
    df_maison_74 = df_maison[df_maison$surface_habitable_logement <= 350 & 
                               df_maison$code_dept == "74", ]
    
    # Si le filtre départemental n'inclut pas le 74, le graphique sera vide (ou si pas de données)
    if(nrow(df_maison_74) == 0 || input$code_postal_filtre == "73") {
      return(NULL) # Ne rien afficher
    }
    
    ggplot(df_maison_74, aes(x = surface_habitable_logement)) +
      geom_histogram(
        breaks = seq(0, 350, length.out = 8), 
        fill = "#A569BD", # Nouvelle couleur pour la distinction
        color = "black"
      ) +
      scale_x_continuous(limits = c(0, 350)) +
      scale_y_continuous(limits = c(0, max(2000, max(table(cut(df_maison_74$surface_habitable_logement, breaks = seq(0, 350, length.out = 8))))*1.1, na.rm = TRUE))) +
      labs(
        title = "Répartition des surfaces habitables des maisons du 74",
        x = "Surface habitable (m²)",
        y = "Effectifs"
      ) +
      theme_light(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  
  # --- 4. Output pour la Carte Leaflet ---
  output$dpe_map <- renderLeaflet({
    data <- map_data() # Utiliser les données converties et filtrées
    
    # Si le data frame est vide après le filtrage/conversion, affichez une carte centrée
    if(nrow(data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 6.4, lat = 45.7, zoom = 9) %>%
               addPopups(lng = 6.4, lat = 45.7, 
                         popup = "Aucune donnée de localisation valide à afficher avec les filtres actuels."))
    }
    
    # Définir les couleurs DPE pour la visualisation
    dpe_colors <- c("A" = "#008000", "B" = "#339900", "C" = "#66B200", 
                    "D" = "#FFCC00", "E" = "#FF9933", "F" = "#FF6666", "G" = "#CC0000")
    
    # S'assurer que 'etiquette_dpe' est un facteur pour un bon mappage des couleurs
    data$etiquette_dpe <- factor(data$etiquette_dpe, levels = names(dpe_colors))
    
    # Créez le contenu des popups (fenêtres d'information au clic)
    content <- paste(sep = "<br/>",
                     paste("<b>DPE:</b>", data$etiquette_dpe),
                     paste("Surface:", data$surface_habitable_logement, "m²"),
                     paste("Type:", data$type_batiment)
    )
    
    # Création de la carte
    leaflet(data) %>%
      addTiles() %>% # Ajout du fond de carte OpenStreetMap
      
      # Centrer la vue sur la zone où se trouvent les points
      fitBounds(lng1 = min(data$longitude), lat1 = min(data$latitude), 
                lng2 = max(data$longitude), lat2 = max(data$latitude)) %>%
      
      # Ajout des marqueurs circulaires avec l'option de clustering
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = content, 
        radius = 5,
        color = "black", 
        fillColor = ~dpe_colors[etiquette_dpe], # Utilisation des couleurs DPE
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.7,
        clusterOptions = markerClusterOptions() # Regroupement des marqueurs
      ) %>%
      # Ajouter une légende pour les couleurs DPE
      addLegend(position = "bottomright",
                colors = dpe_colors[names(dpe_colors) %in% data$etiquette_dpe],
                labels = names(dpe_colors)[names(dpe_colors) %in% data$etiquette_dpe],
                title = "Étiquette DPE")
  })
  
  # --- 5. Output pour la Corrélation ---
  output$Correlation_ges_dpe <- renderPlot({
    
    # Utiliser les données filtrées
    df_filtered <- filtered_data()
    
    # S'assurer que les étiquettes sont des facteurs ordonnés pour le graphique
    dpe_levels <- c("A", "B", "C", "D", "E", "F", "G")
    df_filtered$etiquette_dpe <- factor(df_filtered$etiquette_dpe, levels = dpe_levels)
    df_filtered$etiquette_ges <- factor(df_filtered$etiquette_ges, levels = dpe_levels)
    
    ggplot(df_filtered, aes(x = etiquette_dpe, y = etiquette_ges)) +
      # Utilisation de geom_count pour visualiser la densité de points
      geom_count(aes(size = after_stat(n)), color = "#1a5276", alpha = 0.8) +
      scale_size_area(max_size = 18) +
      labs(
        title = "Corrélation entre Étiquette DPE (Énergie) et GES (Climat)",
        x = "Étiquette DPE (Consommation Énergétique)",
        y = "Étiquette GES (Émissions de Gaz à Effet de Serre)",
        size = "Effectif"
      ) +
      theme_light(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        # Centrer les titres d'axes si possible
        axis.text = element_text(face = "bold")
      ) +
      # Ajout d'une échelle de couleur pour l'effet de compte (si désiré)
      scale_fill_gradient(low = "lightgray", high = "red")
  })
}

# --- Exécution de l'application ---
shinyApp(ui = ui, server = server)
