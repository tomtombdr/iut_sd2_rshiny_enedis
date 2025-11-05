# app.R
# install.packages(c("ggplot2","leaflet","shiny","dplyr", "sf")) # Packages nécessaires pour la création des graphiques
library(ggplot2)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer) # Ajout pour les palettes de couleurs si nécessaire
library(DT)

# S'assurez que le fichier CSV est dans le bon chemin
df_total <- read.csv2(file = "DATA/données_projet_DPE.csv", stringsAsFactors = FALSE)

## Préparation des données pour les filtres 

# S'assurer que 'code_insee_ban' est en caractère
df_total$code_insee_ban <- as.character(df_total$code_insee_ban)

# Création d'une colonne pour le code départemental (les deux premiers chiffres)
df_total$code_dept <- substr(df_total$code_insee_ban, 1, 2)

# Définition des options pour le filtre "neufancien"
neufancien_choices <- c("Les deux" = "les_deux", "Ancien" = "ancien", "Neuf" = "neuf")

## Définition de l'Interface Utilisateur (UI) 
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Présentation du DPE sur les logements neufs et existants en Savoie et Haute-Savoie"),
  
  # Utilisation d'un layout avec barre latérale
  sidebarLayout(
    
    # Panneau de la barre latérale pour les filtres (commun à tous les onglets)
    sidebarPanel(
      width = 3, # Rendre la sidebar plus fine (3/12 de la largeur)

      # Ajout logo Enedis
      tags$img(
        src = "https://www.plogonnec.fr/wp-content/uploads/2022/04/enedis-logo-D7DA244D2C-seeklogo.com_.png",
        width = "100%",     
        style = "margin-bottom: 5px; border-radius: 2px;"
      ),
      h4("Filtres de Données"),
      
      # 1. Filtre par code départemental
      selectInput("code_postal_filtre",
                  "Filtrer par Département (Code INSEE):",
                  choices = c("Les deux" = "tous", 
                              "Savoie (73)" = "73", 
                              "Haute-Savoie (74)" = "74"),
                  selected = "tous"), # 'tous' est la valeur par défaut
      
      # 2. Filtre par type de logement (Neuf/Ancien)
      selectInput("neufancien_filtre",
                  "Type de Logement:",
                  choices = neufancien_choices,
                  selected = "les_deux"), # 'les_deux' est la valeur par défaut
      
      hr(), # Ligne de séparation
      p(em("Les graphiques et la carte ci-dessous sont mis à jour en fonction de ces filtres."))
    ),
    
    # Panneau principal pour les onglets
    mainPanel(
      width = 9, # Augmenter la largeur du panneau principal (9/12 de la largeur)
      # Création des onglets (tabsetPanel)
      tabsetPanel(
        
        # Onglet 1 : Comparaison territoire 
        tabPanel("Comparaison territoire",
                 h3("Répartition des surfaces habitables des maisons"),
                 # Un seul plot pour le 73 et le 74
                 plotOutput("Répartition_surface_maison"),
                 h3("Répartition des surfaces habitables des appartements"),
                 plotOutput("Répartition_surface_appartement")),
        
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
        
        # Onglet 3 : Analyse DPE
        tabPanel("Analyse DPE",
                 h3("Corrélation entre les Étiquettes Énergie et Climat"),
                 plotOutput("Correlation_ges_dpe"),
                 plotOutput("Repartition_dpe_classe_par_departement")
        ),
        
        # Onglet 4 : Contexte
        tabPanel("Contexte",
                 h3("Tableau récapitulatif des champs utilisés"),
                 DT::dataTableOutput("table_doc"),
                 
                 tags$br(),
                 
                 h5("Liens vers les bases de données de l'ADEME", style = "font-weight: bold;"),
                 tags$a(
                   href = "https://data.ademe.fr/datasets/dpe03existant",
                   target = "_blank",
                   tagList(
                     icon("database"),
                     " Source : ADEME (base DPE logements existants)"
                   ),
                   style = "color:#0066cc; font-weight:bold; text-decoration:none;"
                 ),
                 
                 tags$br(),  # saut de ligne
                 tags$br(),
                 
                 tags$a(
                   href = "https://data.ademe.fr/datasets/dpe02neuf",
                   target = "_blank",
                   tagList(
                     icon("database"),
                     " Source : ADEME (base DPE logements neufs)"
                   ),
                   style = "color:#0066cc; font-weight:bold; text-decoration:none;"
                 )
        )
      )
    )
  )
)

## Définition de l'Interface Server (server) 

server <- function(input, output) {
  
  # Filtrage des Données 
  filtered_data <- reactive({
    data <- df_total
    
    # FILTRE 1: Code Postal (Département)
    # Si 'tous' est sélectionné, on ne filtre pas
    if (input$code_postal_filtre != "tous") {
      data <- data %>%
        filter(code_dept == input$code_postal_filtre)
    }
    
    # FILTRE 2: Neuf / Ancien
    # Si 'les_deux' est sélectionné, on ne filtre pas
    if (input$neufancien_filtre != "les_deux") {
      # Le filtre utilise les valeurs brutes "ancien" et "neuf" du jeu de données
      data <- data %>%
        filter(neufancien == input$neufancien_filtre)
    }
    
    return(data)
  })
  
  
  # Prépation des données de la carte (coordonnées) 
  map_data <- reactive({
    # Utiliser les données filtrées
    data <- filtered_data()
    
    # Conversion des coordonnées X/Y en numérique pour Leaflet.
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
    
    # Conversion du Lambert 93 (EPSG:2154) au WGS84 (EPSG:4326)
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
    
    # FILTRAGE DES POINTS ABERRANTS (Hors de France métropolitaine)
    data_filtered <- data_wgs84 %>%
      filter(longitude >= -5 & longitude <= 10 & 
               latitude >= 42 & latitude <= 51)
    
    # Retourner les données filtrées
    data_filtered %>%
      select(longitude, latitude, etiquette_dpe, surface_habitable_logement, type_batiment, everything())
  })
  
  
  # Output pour l'Histogramme de Répartition des Surfaces (73 et/ou 74) 
  output$Répartition_surface_maison <- renderPlot({
    
    # Utiliser les données filtrées
    df_filtered <- filtered_data()
    
    # Filtrer uniquement les maisons et les surfaces raisonnables
    df_maison = df_filtered %>%
      filter(type_batiment == "maison",
             surface_habitable_logement <= 350)
    
    # Vérification si des données existent
    if(nrow(df_maison) == 0) {
      # Retourner un graphique vide ou un message d'erreur
      return(
        ggplot() +
          labs(title = "Aucune donnée de maison disponible avec les filtres actuels.") +
          theme_void()
      )
    }
    
    # Préparation du titre en fonction du filtre départemental
    dept_name <- switch(input$code_postal_filtre,
                        "73" = "Savoie (73)",
                        "74" = "Haute-Savoie (74)",
                        "tous" = "Savoie (73) et Haute-Savoie (74)")
    
    plot_title <- paste("Répartition des surfaces habitables des maisons en", dept_name)
    
    # histogramme répartition surface habitable maison
    p <- ggplot(df_maison, aes(x = surface_habitable_logement)) +
      geom_histogram(
        breaks = seq(0, 350, length.out = 11),
        fill = "red",
        color = "black"
      )
    
    # Mise en forme commune
    p +
      scale_x_continuous(limits = c(0, 350)) +
      labs(
        title = plot_title,
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
  
  output$Répartition_surface_appartement <- renderPlot({
    
    # Utiliser les données filtrées
    df_filtered <- filtered_data()
    
    # Filtrer uniquement les maisons et les surfaces raisonnables
    df_appartement = df_filtered %>%
      filter(type_batiment == "appartement",
             surface_habitable_logement <= 250)
    
    # Vérification si des données existent
    if(nrow(df_appartement) == 0) {
      # Retourner un graphique vide ou un message d'erreur
      return(
        ggplot() +
          labs(title = "Aucune donnée de maison disponible avec les filtres actuels.") +
          theme_void()
      )
    }
    
    # Préparation du titre en fonction du filtre départemental
    dept_name <- switch(input$code_postal_filtre,
                        "73" = "Savoie (73)",
                        "74" = "Haute-Savoie (74)",
                        "tous" = "Savoie (73) et Haute-Savoie (74)")
    
    plot_title <- paste("Répartition des surfaces habitables des appartements en", dept_name)
    
    # histogramme répartition surface habitable appartement
    p <- ggplot(df_appartement, aes(x = surface_habitable_logement)) +
      geom_histogram(
        breaks = seq(0, 250, length.out = 11),
        fill = "red",
        color = "black"
      )
    
    # Mise en forme commune
    p +
      scale_x_continuous(limits = c(0, 250)) +
      labs(
        title = plot_title,
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
  
  # Output pour la Carte Leaflet
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
    
    # Créez le contenu des popups 
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
  
  # Output pour la Corrélation 
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
  
  # Répartition des DPE par département
  output$Repartition_dpe_classe_par_departement <- renderPlot({
    
    df <- filtered_data()
    
    # Vérification que la colonne "etiquette_dpe" existe
    if(!"etiquette_dpe" %in% colnames(df)) {
      validate(need(FALSE, "Aucune colonne 'etiquette_dpe' trouvée dans les données."))
    }
    
    # Intégration des données
    df_summary <- df %>%
      group_by(code_dept, etiquette_dpe) %>%
      summarise(nb_logements = n(), .groups = "drop")
    
    # Création du graphique
    ggplot(df_summary, aes(x = etiquette_dpe, y = nb_logements, fill = code_dept)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set1", name = "Département") +
      labs(
        title = "Répartition des classes DPE par département",
        x = "Classe DPE",
        y = "Nombre de logements"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
  
  output$table_doc <- DT::renderDataTable({
    
    # Création du tableau de description des champs
    
    doc <- data.frame(
      Champ = names(df_total),
      Description = c(
        "Nom de la commune du logement",
        "Code insee du logement",
        "Année de construction du logement",
        "Coordonnée géographique x des logements",
        "Type de batiment (maison/appartement)",
        "Type d'installation des eaux chaude sanitaire (individuel/collectif)",
        "Date de reception du DPE",
        "Notation de la production de gaz à effet de serre (A à G)",
        "Coordonnée géographique y des logements",
        "Type d'installation du chauffage (individuel/collectif)",
        "Code postal du logement",
        "Surface habitable du logement (m²)",
        "Hauteur sous plafond du logement (m)",
        "Notation du DPE (A à G)",
        "Score du logement",
        "Catégorie d'énergie utilisée par le logement",
        "Classe d'âge du logement (neuf/ancien)",
        "Code du département"
      )
    )
    
    # Rend le tableau intéractif
    datatable(doc, options = list(pageLength = 20))
  })
  
}

# Exécution de l'application 
shinyApp(ui = ui, server = server)
