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

# S'assurer que le fichier CSV est dans le bon chemin
# ATTENTION: Assurez-vous que le chemin est correct sur votre machine!
df_total <- read.csv2(file = "../DATA/données_projet_DPE.csv", stringsAsFactors = FALSE)

## Préparation des données pour les filtres
df_total$code_insee_ban <- as.character(df_total$code_insee_ban)
df_total$code_postal_ban <- as.character(df_total$code_postal_ban)
# Conversion des colonnes avec virgule comme séparateur décimal
df_total$surface_habitable_logement <- as.numeric(gsub(",", ".", df_total$surface_habitable_logement))
# NOUVEAU: Conversion de la hauteur sous plafond
df_total$hauteur_sous_plafond <- as.numeric(gsub(",", ".", df_total$hauteur_sous_plafond))
# Ajout des conversions pour les coordonnées si elles sont utilisées
df_total$coordonnee_cartographique_x_ban <- as.numeric(gsub(",", ".", df_total$coordonnee_cartographique_x_ban))
df_total$coordonnee_cartographique_y_ban <- as.numeric(gsub(",", ".", df_total$coordonnee_cartographique_y_ban))

df_total$code_dept <- substr(df_total$code_insee_ban, 1, 2)
neufancien_choices <- c("Les deux" = "les_deux", "Ancien" = "ancien", "Neuf" = "neuf")
dpe_levels <- c("A", "B", "C", "D", "E", "F", "G")

# Choix pour les listes déroulantes du nuage de points
scatter_choices <- c(
  "Surface habitable (m²)" = "surface_habitable_logement",
  "Année de construction" = "annee_construction",
  "Hauteur sous plafond (m)" = "hauteur_sous_plafond",
  "Étiquette DPE" = "etiquette_dpe",
  "Étiquette GES" = "etiquette_ges",
  "Type de bâtiment" = "type_batiment",
  "Classe d'âge (Neuf/Ancien)" = "neufancien"
)


## Définition de l'Interface Utilisateur (UI)

# --- NOUVELLE PARTIE POUR INJECTER LE CSS DANS BSLIB ---

# 1. Lire le contenu du fichier CSS personnalisé (doit être dans le dossier www)
# Vérifiez que 'www/style.css' existe!
if (file.exists("www/style.css")) {
  custom_css_rules <- readLines("www/style.css", encoding = "UTF-8") %>%
    paste(collapse = "\n")
} else {
  warning("Le fichier 'www/style.css' n'a pas été trouvé. Les styles personnalisés ne seront pas appliqués.")
  custom_css_rules <- "" # Assure que la variable n'est pas vide en cas d'erreur
}


# 2. Définir un thème de base (obligatoire pour bslib)
my_theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo" # Thème par défaut
)

# 3. Injecter le CSS personnalisé dans le thème bslib
my_theme <- bs_add_rules(my_theme, custom_css_rules)

# --------------------------------------------------------

# DÉFINITION DES CRÉDENTIELS POUR shinymanager
usersapp <- data.frame(
  user = c("admin"),
  password = c("admin"),
  admin = TRUE,
  comment = "page d'identification pour acceder à l'application",
  stringsAsFactors = FALSE
)

# L'UI principale est renommée 'ui_content'
ui_content <- fluidPage( 
  
  # REMPLACÉ: Suppression du tags$head(includeCSS())
  
  # THÈME BSLIB RÉACTIF (Contient maintenant les règles CSS personnalisées)
  theme = my_theme,	
  
  # Titre de l'application
  titlePanel(
    tags$div(
      "Présentation du DPE sur les logements neufs et existants en Savoie et Haute-Savoie", 
      class = "title" # Cette classe est stylisée dans style.css
    )
  ),
  
  # Utilisation d'un layout avec barre latérale
  sidebarLayout(
    
    # Panneau de la barre latérale pour les filtres (commun à tous les onglets)
    sidebarPanel(
      width = 3,
      class = "sidebar-panel", # Ajout d'une classe pour styliser si besoin
      
      # CONTRÔLE DE SÉLECTION DU THÈME	
      selectInput("theme_selector", "Changer de Thème :",
                  choices = c(
                    "Cosmo" = "cosmo",
                    "Darkly" = "darkly",
                    "Lumen" = "lumen",
                    "Superhero" = "superhero",
                    "Minty" = "minty"
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
      p(em("Les graphiques et la carte des différents onglets sont mis à jour en fonction de ces filtres."))
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
                 plotOutput("boxplot_surface_par_dpe"),	
                 
                 # --- NUAGE DE POINTS INTERACTIF ---
                 hr(),
                 h3("Analyse de Corrélation Personnalisée (Nuage de Points)"),
                 downloadButton("download_scatter_plot", "Exporter en PNG 🖼️"),
                 fluidRow(
                   column(6,
                          selectInput("scatter_x_var", "Variable X (Abscisse) :",
                                      choices = scatter_choices,
                                      selected = "surface_habitable_logement")
                   ),
                   column(6,
                          selectInput("scatter_y_var", "Variable Y (Ordonnée) :",
                                      choices = scatter_choices,
                                      selected = "annee_construction")
                   )
                 ),
                 # AFFICHAGE DU COEFFICIENT DE CORRÉLATION (MAINTENANT AVEC renderUI)
                 htmlOutput("correlation_coefficient_output"), 
                 
                 plotOutput("scatter_plot_output") # ID pour le nuage de points
                 # --------------------------------------------------------
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
  
  # LOGIQUE DE CHANGEMENT DE THÈME
  observeEvent(input$theme_selector, {
    # Charger le thème sélectionné par l'utilisateur
    new_theme <- bs_theme_update(my_theme, bootswatch = input$theme_selector)
    
    # Appliquer le nouveau thème (qui inclut toujours le custom CSS)
    session$setCurrentTheme(new_theme)
  })
  
  # Génération de l'UI pour le filtre par Code Postal (ID: code_postal_filtre)
  output$code_postal_ui <- renderUI({
    # Rendre la fonction robuste en gérant les NA et les chaînes vides
    valid_codes <- df_total$code_postal_ban[!is.na(df_total$code_postal_ban) & df_total$code_postal_ban != ""]
    
    # Récupérer la sélection du département
    dept_selection <- input$code_dept_filtre
    
    # Filtrer la liste des codes postaux en fonction de la sélection du département
    if (dept_selection == "73") {
      choices <- sort(unique(valid_codes[startsWith(valid_codes, "73")]))
    } else if (dept_selection == "74") {
      choices <- sort(unique(valid_codes[startsWith(valid_codes, "74")]))
    } else {
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
    
    # FILTRE 4: Classe DPE (CheckBox) - Utilisé sur l'onglet 2 (affecte aussi l'onglet 3)
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
    
    # Conversion des coordonnées X/Y en numérique (déjà fait dans le setup global, mais on réutilise pour la robustesse)
    data <- data %>%
      mutate(
        x_lambert = coordonnee_cartographique_x_ban, # Colonne déjà convertie dans le setup
        y_lambert = coordonnee_cartographique_y_ban  # Colonne déjà convertie dans le setup
      )
    
    # Filtrer les lignes avec des coordonnées NA ou non valides après la première conversion
    data <- data %>%	
      filter(!is.na(x_lambert) & !is.na(y_lambert) & !is.nan(x_lambert) & !is.nan(y_lambert))
    
    # S'il ne reste aucune donnée valide, on retourne un data frame vide
    if(nrow(data) == 0) {
      return(data.frame(longitude = numeric(0), latitude = numeric(0), etiquette_dpe = character(0)))
    }
    
    # Conversion du Lambert 93 (EPSG:2154) au WGS84 (EPSG:4326)
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
    
    # Filtrer les valeurs de surface trop extrêmes pour une meilleure visualisation (par exemple > 500m²)
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
      
      # CODE PRINCIPAL POUR LA BOÎTE À MOUSTACHES
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
  
  
  # LOGIQUE DE GÉNÉRATION DU NUAGE DE POINTS INTERACTIF (dans l'onglet 3)
  generate_scatter_plot <- reactive({
    
    # Données filtrées
    df_plot <- filtered_data() %>%
      select(input$scatter_x_var, input$scatter_y_var) %>%
      # Supprimer les NA pour le graphique
      na.omit() 
    
    # Filtrage des outliers extrêmes pour les variables numériques pour une meilleure visualisation
    if (input$scatter_x_var %in% c("surface_habitable_logement")) {
      df_plot <- df_plot %>% filter(df_plot[[input$scatter_x_var]] < 500)
    }
    if (input$scatter_y_var %in% c("surface_habitable_logement")) {
      df_plot <- df_plot %>% filter(df_plot[[input$scatter_y_var]] < 500)
    }
    
    
    # Vérification si des données existent
    if(nrow(df_plot) == 0) {
      return(
        ggplot() +
          labs(title = "Aucune donnée disponible avec les filtres et variables sélectionnés.") +
          theme_void()
      )
    }
    
    # Définir les variables d'axe
    x_var_name <- input$scatter_x_var
    y_var_name <- input$scatter_y_var
    
    # Récupérer les labels (utiliser les noms de la liste scatter_choices)
    x_label <- names(scatter_choices[scatter_choices == x_var_name])
    y_label <- names(scatter_choices[scatter_choices == y_var_name])
    
    # Définir les variables pour le graphique
    df_plot$x <- df_plot[[x_var_name]]
    df_plot$y <- df_plot[[y_var_name]]
    
    # DPE/GES levels pour ordonner correctement
    dpe_levels <- c("A", "B", "C", "D", "E", "F", "G")
    
    # Mettre en facteur si nécessaire pour un meilleur rendu
    if (x_var_name %in% c("etiquette_dpe", "etiquette_ges")) {
      df_plot$x <- factor(df_plot$x, levels = dpe_levels)
    } else if (x_var_name %in% c("type_batiment", "neufancien")) {
      df_plot$x <- factor(df_plot$x)
    }
    
    if (y_var_name %in% c("etiquette_dpe", "etiquette_ges")) {
      df_plot$y <- factor(df_plot$y, levels = dpe_levels)
    } else if (y_var_name %in% c("type_batiment", "neufancien")) {
      df_plot$y <- factor(df_plot$y)
    }
    
    # Création du graphique de base
    p <- ggplot(df_plot, aes(x = x, y = y)) +
      labs(
        title = paste("Corrélation entre", x_label, "et", y_label),
        x = x_label,
        y = y_label
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold")
      )
    
    # Logique de visualisation:
    
    # 1. Les deux axes sont numériques
    if (is.numeric(df_plot$x) && is.numeric(df_plot$y)) {
      p <- p +
        geom_point(alpha = 0.5, color = "#1a5276", size = 2) +
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")
    }
    # 2. Un ou les deux axes sont catégoriels/discrets (utiliser geom_count ou jitter)
    else {
      # Si les deux sont des étiquettes (DPE, GES, Type, Neuf/Ancien)
      if (is.factor(df_plot$x) && is.factor(df_plot$y)) {
        p <- p +
          geom_count(aes(size = after_stat(n)), color = "#1a5276", alpha = 0.8) +
          scale_size_area(max_size = 18) +
          labs(size = "Effectif")
      } else {
        # Sinon, utiliser jitter pour visualiser la répartition (numérique vs catégoriel)
        p <- p +
          geom_point(position = position_jitter(width = 0.3, height = 0.3), 
                     alpha = 0.5, color = "#1a5276", size = 2)
      }
    }
    
    return(p)
  })
  
  # Fonction pour calculer et afficher le coefficient de corrélation
  calculate_correlation <- reactive({
    req(input$scatter_x_var, input$scatter_y_var)
    
    df_corr <- filtered_data()
    x_var <- input$scatter_x_var
    y_var <- input$scatter_y_var
    
    x_data <- df_corr[[x_var]]
    y_data <- df_corr[[y_var]]
    
    # Exclure les variables purement nominales (non ordonnées) pour Spearman
    excluded_vars <- c("type_batiment", "neufancien")
    
    if (x_var %in% excluded_vars || y_var %in% excluded_vars) {
      output_text <- paste(
        "<div style='border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f8f8f8; margin-top: 10px;'>",
        "<b>Coefficient de Corrélation:</b> Non calculable (variable nominale sélectionnée).",
        "</div>"
      )
      # FIX: Retourner le texte enveloppé dans HTML()
      return(HTML(output_text))
    }
    
    # Conversion de rang pour les variables DPE/GES ordonnées
    dpe_levels <- c("A", "B", "C", "D", "E", "F", "G")
    
    if (x_var %in% c("etiquette_dpe", "etiquette_ges")) {
      # Convertir les étiquettes en rang numérique (1 à 7) pour le calcul de corrélation
      x_data <- as.numeric(factor(x_data, levels = dpe_levels, ordered = TRUE))
    }
    if (y_var %in% c("etiquette_dpe", "etiquette_ges")) {
      y_data <- as.numeric(factor(y_data, levels = dpe_levels, ordered = TRUE))
    }
    
    # Enlever les NA pour la corrélation
    valid_data <- na.omit(data.frame(x_data, y_data))
    
    if (nrow(valid_data) < 2) {
      output_text <- paste(
        "<div style='border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f8f8f8; margin-top: 10px;'>",
        "<b>Coefficient de Corrélation:</b> Impossible (moins de 2 points de données valides).",
        "</div>"
      )
      # FIX: Retourner le texte enveloppé dans HTML()
      return(HTML(output_text))
    }
    
    # Calculer le coefficient de corrélation de Spearman
    cor_value <- cor(valid_data$x_data, valid_data$y_data, method = "spearman", use = "complete.obs")
    
    # Récupérer les labels
    x_label <- names(scatter_choices[scatter_choices == x_var])
    y_label <- names(scatter_choices[scatter_choices == y_var])
    
    # Affichage formaté
    formatted_cor <- format(round(cor_value, 3), nsmall = 3)
    
    # Définir la couleur en fonction de la force de la corrélation (standard R > 0.5)
    cor_color <- ifelse(abs(cor_value) > 0.6, "#c0392b", 
                        ifelse(abs(cor_value) > 0.3, "#f39c12", "#27ae60"))
    
    output_text <- paste(
      "<div style='border: 1px solid #1a5276; padding: 10px; border-radius: 5px; background-color: #f0f8ff; margin-top: 10px;'>",
      "<b>Coefficient de Corrélation :</b>", 
      "<span style='color:", cor_color, "; font-size: 1.1em; font-weight: bold;'>",
      formatted_cor,
      "</span>",
      " (entre ", x_label, " et ", y_label, ")",
      "</div>"
    )
    
    # FIX: Retourner le texte enveloppé dans HTML() pour un rendu correct
    return(HTML(output_text))
  })
  
  # Rendu de l'output du coefficient
  output$correlation_coefficient_output <- renderUI({ 
    calculate_correlation() 
  })
  
  # Rendu du nuage de points
  output$scatter_plot_output <- renderPlot({ generate_scatter_plot() })
  
  
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
  
  # 5. NOUVEAU: Exportation du nuage de points
  output$download_scatter_plot <- downloadHandler(
    filename = function() {
      paste("nuage_points_", input$scatter_x_var, "_vs_", input$scatter_y_var, "-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = generate_scatter_plot(), device = "png", width = 10, height = 7, units = "in")
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