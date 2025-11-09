# app.R
# install.packages(c("ggplot2","rsconnect","leaflet","shiny","dplyr", "sf", "RColorBrewer", "DT", "bslib", "shinymanager")) 

library(ggplot2)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(RColorBrewer)
library(DT)
library(bslib)
library(rsconnect)
library(shinymanager) # Package pour la sécurité

# S'assurez que le fichier CSV est dans le bon chemin
df_total <- read.csv2(file = "../DATA/données_projet_DPE.csv", stringsAsFactors = FALSE)

## Préparation des données pour les filtres

# S'assurer que 'code_insee_ban' est en caractère
df_total$code_insee_ban <- as.character(df_total$code_insee_ban)

# S'assurer que 'code_postal_ban' est en caractère (essentiel pour startsWith et éviter les erreurs)
df_total$code_postal_ban <- as.character(df_total$code_postal_ban)

# Conversion de la surface en numérique (R gère la virgule si read.csv2 est utilisé, mais une conversion explicite est plus sûre)
df_total$surface_habitable_logement <- as.numeric(gsub(",", ".", df_total$surface_habitable_logement))

# Création d'une colonne pour le code départemental (les deux premiers chiffres)
df_total$code_dept <- substr(df_total$code_insee_ban, 1, 2)

# Définition des options pour le filtre "neufancien"
neufancien_choices <- c("Les deux" = "les_deux", "Ancien" = "ancien", "Neuf" = "neuf")

# Définition des options pour le filtre "DPE"
dpe_levels <- c("A", "B", "C", "D", "E", "F", "G")


## Définition de l'Interface Utilisateur (UI)

# Définir un thème de base (obligatoire pour bslib)
my_theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo" # Thème par défaut
)

# DÉFINITION DES CRÉDENTIELS POUR shinymanager
usersapp <- data.frame(
  user = c("admin"), # Utilisation de c() pour être sûr
  password = c("admin"), # Utilisation de c() pour être sûr
  admin = TRUE,
  comment = "page d'identification pour acceder à l'application",
  stringsAsFactors = FALSE # Correction du nom
)

# L'UI principale est renommée 'ui_content'
ui_content <- fluidPage( 
  
  # THÈME BSLIB RÉACTIF 
  theme = my_theme,	
  
  # Titre de l'application
  titlePanel("Présentation du DPE sur les logements neufs et existants en Savoie et Haute-Savoie"),
  
  # Utilisation d'un layout avec barre latérale
  sidebarLayout(
    
    # Panneau de la barre latérale pour les filtres (commun à tous les onglets)
    sidebarPanel(
      width = 3,
      
      # CONTRÔLE DE SÉLECTION DU THÈME 
      selectInput("theme_selector", "Changer de Thème :",
                  choices = c(
                    "Cosmo (Clair Moderne)" = "cosmo",
                    "Darkly (Sombre)" = "darkly",
                    "Lumen (Minimaliste)" = "lumen",
                    "Superhero (Sombre Audacieux)" = "superhero",
                    "Minty (Clair Vert)" = "minty"
                  ),
                  selected = "cosmo"),
      hr(),

      # Ajout logo Enedis
      tags$img(
        src = "https://www.plogonnec.fr/wp-content/uploads/2022/04/enedis-logo-D7DA244D2C-seeklogo.com_.png",
        width = "100%",	 	
        style = "margin-bottom: 5px; border-radius: 2px;"
      ),
      h4("Filtres de Données"),
      
      # 1. Filtre par code départemental (ID: code_dept_filtre)
      selectInput("code_dept_filtre",	
                  "Filtrer par Département (Code INSEE):",
                  choices = c("Les deux" = "tous",	
                              "Savoie (73)" = "73",	
                              "Haute-Savoie (74)" = "74"),
                  selected = "tous"),	
      
      # ESPACE RÉSERVÉ POUR LE FILTRE DE CODE POSTAL DYNAMIQUE
      uiOutput("code_postal_ui"),
      
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
      width = 9,	
      # Création des onglets (tabsetPanel)
      tabsetPanel(
        
        # Onglet 1 : Comparaison territoire	
        tabPanel("Comparaison territoire",
                 h3("Répartition des surfaces habitables des maisons"),
                 # AJOUT DU BOUTON DE TÉLÉCHARGEMENT POUR LE GRAPHIQUE 1
                 downloadButton("download_surface_maison", "Exporter en PNG 🖼️"), 
                 plotOutput("Répartition_surface_maison"),
                 hr(),
                 h3("Répartition des surfaces habitables des appartements"),
                 # AJOUT DU BOUTON DE TÉLÉCHARGEMENT POUR LE GRAPHIQUE 2
                 downloadButton("download_surface_appartement", "Exporter en PNG 🖼️"),
                 plotOutput("Répartition_surface_appartement")),
        
        # Onglet 2 : Carte avec clustering
        tabPanel("Carte avec clustering",	
                 fluidRow(
                   column(3,
                          # Filtre par classe DPE pour la carte (Checkbox)
                          h4("Filtrer par Classe DPE"),
                          checkboxGroupInput("dpe_classe_filtre",
                                             label = NULL,	
                                             choices = dpe_levels,
                                             selected = dpe_levels)
                   ),
                   column(9,
                          h3("Localisation de tous les logements (Clustering)", align = "center", style = "margin-top: 20px; color: #1a5276;"),
                          # Output pour la carte Leaflet
                          leafletOutput("dpe_map", height = "700px")
                   )
                 )
        ),
        
        # Onglet 3 : Analyse DPE
        tabPanel("Analyse DPE",
                 # SLIDER POUR LA SUPERFICIE (MIS À JOUR)
                 fluidRow(
                   column(12,	
                          sliderInput("surface_filtre",
                                      "Filtrer par Superficie Habitable :",
                                      min = 0,	
                                      max = 500,
                                      value = c(0, 500),
                                      step = 1,
                                      dragRange = TRUE)
                   )
                 ),
                 
                 h3("Corrélation entre les Étiquettes Énergie et Climat"),
                 # AJOUT DU BOUTON DE TÉLÉCHARGEMENT POUR LE GRAPHIQUE 3
                 downloadButton("download_correlation", "Exporter en PNG 🖼️"),
                 plotOutput("Correlation_ges_dpe"),
                 hr(),
                 
                 # CHANGEMENT : REMPLACEMENT DE L'HISTOGRAMME EN BARRES PAR LA BOÎTE À MOUSTACHES
                 h3("Distribution de la Surface Habitable par Classe DPE"),
                 # AJOUT DU BOUTON DE TÉLÉCHARGEMENT POUR LE GRAPHIQUE 4
                 downloadButton("download_boxplot_dpe", "Exporter en PNG 🖼️"),
                 plotOutput("boxplot_surface_par_dpe")	
                 # NOUVEL ID
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
                 
                 tags$br(),	
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

# APPLICATION DE LA SÉCURITÉ : L'UI publique devient l'interface de connexion
ui <- secure_app(ui_content)

## Définition de l'Interface Server (server)

server <- function(input, output, session) {
  
  # Initialisation du module de sécurité : VÉRIFIE LES IDENTIFIANTS
  res_auth <- secure_server(
    check_credentials = check_credentials(usersapp) # <--- UTILISATION DIRECTE DE usersapp
  )
  
  # Supprimé : output$auth_output (affichait les détails de connexion, non nécessaire dans l'app finale)
  
  #  LOGIQUE DE CHANGEMENT DE THÈME
  # Le reste de votre logique de serveur
  
  observeEvent(input$theme_selector, {
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$theme_selector)
    )
  })
  
  # Génération de l'UI pour le filtre par Code Postal (ID: code_postal_filtre)
  output$code_postal_ui <- renderUI({
    
    # Rendre la fonction robuste en gérant les NA et les chaînes vides
    valid_codes <- df_total$code_postal_ban[!is.na(df_total$code_postal_ban) & df_total$code_postal_ban != ""]
    
    # Récupérer la sélection du département
    dept_selection <- input$code_dept_filtre
    
    # Filtrer la liste des codes postaux en fonction de la sélection du département
    if (dept_selection == "73") {
      # Codes postaux qui commencent par '73'
      choices <- sort(unique(valid_codes[startsWith(valid_codes, "73")]))
    } else if (dept_selection == "74") {
      # Codes postaux qui commencent par '74'
      choices <- sort(unique(valid_codes[startsWith(valid_codes, "74")]))
    } else {
      # Si "tous" est sélectionné, on prend tous les codes postaux des deux départements (73 et 74)
      choices <- sort(unique(valid_codes[startsWith(valid_codes, "73") | startsWith(valid_codes, "74")]))
    }
    
    # Ajouter l'option "Toutes les communes" au début de la liste
    choices_with_all <- c("Toutes les communes" = "toutes_communes", choices)
    
    # Création du selectInput pour le code postal
    selectInput("code_postal_filtre",
                "Filtrer par Code Postal:",
                choices = choices_with_all,
                selected = "toutes_communes")
  })
  
  # Filtrage des Données	
  filtered_data <- reactive({
    data <- df_total
    
    # FILTRE 1: Code Départemental (DEPT) - utilise 'code_dept_filtre'
    if (input$code_dept_filtre != "tous") {
      data <- data %>%
        filter(code_dept == input$code_dept_filtre)
    }
    
    # FILTRE 2: Code Postal (CP) - utilise 'code_postal_filtre'
    if (!is.null(input$code_postal_filtre) && input$code_postal_filtre != "toutes_communes") {
      data <- data %>%
        filter(code_postal_ban == input$code_postal_filtre)
    }
    
    # FILTRE 3: Neuf / Ancien
    if (input$neufancien_filtre != "les_deux") {
      data <- data %>%
        filter(neufancien == input$neufancien_filtre)
    }
    
    # FILTRE 4: Classe DPE (CheckBox) - Utilisé sur l'onglet 2
    if (!is.null(input$dpe_classe_filtre) && length(input$dpe_classe_filtre) > 0) {
      data <- data %>%
        filter(etiquette_dpe %in% input$dpe_classe_filtre)
    }	
    
    # FILTRE 5: Superficie (Slider) - Utilisé sur l'onglet 3
    if (!is.null(input$surface_filtre)) {
      min_surface <- input$surface_filtre[1]
      max_surface <- input$surface_filtre[2]
      
      data <- data %>%
        filter(surface_habitable_logement >= min_surface & surface_habitable_logement <= max_surface)
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
  
  
  # LOGIQUE DE GÉNÉRATION DES GRAPHIQUES (Pour la fonction downloadHandler)
  
  # Fonction pour générer le graphique de répartition des surfaces des maisons
  generate_surface_maison_plot <- reactive({
    
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
    
    # Préparation du titre en fonction du filtre départemental (ID mis à jour)
    dept_name <- switch(input$code_dept_filtre,
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
  
  # Fonction pour générer le graphique de répartition des surfaces des appartements
  generate_surface_appartement_plot <- reactive({
    
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
          labs(title = "Aucune donnée d'appartement disponible avec les filtres actuels.") +
          theme_void()
      )
    }
    
    # Préparation du titre en fonction du filtre départemental (ID mis à jour)
    dept_name <- switch(input$code_dept_filtre,
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
  
  # Fonction pour générer le graphique de corrélation
  generate_correlation_plot <- reactive({
    
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
  
  # Fonction pour générer le graphique de boîte à moustaches
  generate_boxplot_dpe <- reactive({
    
    df <- filtered_data()
    
    # S'assurer que les étiquettes DPE sont des facteurs ordonnés
    dpe_levels <- c("A", "B", "C", "D", "E", "F", "G")
    df$etiquette_dpe <- factor(df$etiquette_dpe, levels = dpe_levels)
    
    # Définir les couleurs DPE (celles utilisées dans la carte pour la cohérence)
    dpe_colors <- c("A" = "#008000", "B" = "#339900", "C" = "#66B200",	
                    "D" = "#FFCC00", "E" = "#FF9933", "F" = "#FF6666", "G" = "#CC0000")
    
    # Filtrer les valeurs de surface trop extrêmes pour une meilleure visualisation (par exemple > 300m²)
    df_filtered_box <- df %>%
      filter(surface_habitable_logement < 500)
    
    # Vérification que des données existent
    if(nrow(df_filtered_box) == 0) {
      return(
        ggplot() +
          labs(title = "Aucune donnée disponible pour la boîte à moustaches (ou surface filtrée).") +
          theme_void()
      )
    }
    
    # Création de la boîte à moustaches
    ggplot(df_filtered_box, aes(x = etiquette_dpe, y = surface_habitable_logement, fill = etiquette_dpe)) +
      
      # --- CODE PRINCIPAL POUR LA BOÎTE À MOUSTACHES ---
      geom_boxplot(outlier.shape = 1) + # Ajouter les points aberrants
      
      # Application des couleurs DPE
      scale_fill_manual(values = dpe_colors, name = "Classe DPE") +
      
      labs(
        title = "Distribution de la Surface Habitable par Classe DPE",
        x = "Classe DPE",
        y = "Surface habitable (m²)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.position = "none" # La légende n'est pas nécessaire si l'axe x est étiqueté
      )
  })
  
  # LOGIQUE DE RENDU DES GRAPHIQUES (Pour l'affichage dans l'UI)
  
  output$Répartition_surface_maison <- renderPlot({ generate_surface_maison_plot() })
  output$Répartition_surface_appartement <- renderPlot({ generate_surface_appartement_plot() })
  output$Correlation_ges_dpe <- renderPlot({ generate_correlation_plot() })
  output$boxplot_surface_par_dpe <- renderPlot({ generate_boxplot_dpe() })
  
  # LOGIQUE D'EXPORTATION (downloadHandler)
  
  # 1. Exportation du graphique de la répartition des surfaces des maisons
  output$download_surface_maison <- downloadHandler(
    filename = function() {
      paste("repartition_surface_maison-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      # Assurez-vous que l'objet plot a une hauteur et une largeur raisonnables
      ggsave(file, plot = generate_surface_maison_plot(), device = "png", width = 10, height = 7, units = "in")
    }
  )
  
  # 2. Exportation du graphique de la répartition des surfaces des appartements
  output$download_surface_appartement <- downloadHandler(
    filename = function() {
      paste("repartition_surface_appartement-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = generate_surface_appartement_plot(), device = "png", width = 10, height = 7, units = "in")
    }
  )
  
  # 3. Exportation du graphique de corrélation DPE/GES
  output$download_correlation <- downloadHandler(
    filename = function() {
      paste("correlation_dpe_ges-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = generate_correlation_plot(), device = "png", width = 10, height = 7, units = "in")
    }
  )
  
  # 4. Exportation du graphique de boîte à moustaches
  output$download_boxplot_dpe <- downloadHandler(
    filename = function() {
      paste("boxplot_surface_dpe-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = generate_boxplot_dpe(), device = "png", width = 10, height = 7, units = "in")
    }
  )
  
  # Reste de la logique du serveur
  
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
      fitBounds(lng1 = min(data$longitude, na.rm = TRUE), lat1 = min(data$latitude, na.rm = TRUE),	
                lng2 = max(data$longitude, na.rm = TRUE), lat2 = max(data$latitude, na.rm = TRUE)) %>%
      
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
  
  # Output pour le Tableau de documentation
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