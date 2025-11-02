# app.R
# install.packages(c("ggplot2","leaflet","shiny","dplyr", "sf"))
library(ggplot2)
library(shiny)
library(leaflet)
library(dplyr)
library(sf) 

df_total <- read.csv2(file = "DATA/données_projet_DPE.csv", stringsAsFactors = FALSE) # stringsAsFactors=FALSE pour faciliter la manipulation

# --- Définition de l'Interface Utilisateur (UI) ---
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Présentation du DPE sur les logements neufs et existants en Savoie et Haute-Savoie"),
  
  # Création des onglets (tabsetPanel)
  tabsetPanel(
    
    # Onglet 1 : Comparaison territoire (Histogrammes modifiés)
    tabPanel("Comparaison territoire",
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
    
    # Onglet 3
    tabPanel("Onglet 3") 
  )
)

server <- function(input, output) {
  
  # Reactive expression pour préparer les données de la carte (coordonnées)
  # Ce bloc effectue la conversion de Lambert 93 (EPSG:2154) vers WGS84 (EPSG:4326) pour Leaflet
  map_data <- reactive({
    data <- df_total 
    
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
      return(data.frame(longitude = numeric(0), latitude = numeric(0), etiquette_dpe = character(0), surface_habitable_logement = numeric(0), type_batiment = character(0)))
    }
    
    # 2. Conversion du Lambert 93 (EPSG:2154) au WGS84 (EPSG:4326)
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
    
    # 3. FILTRAGE DES POINTS ABERRANTS (Pour exclure les points hors de France métropolitaine car 203 dans le pacifique)
    data_filtered <- data_wgs84 %>%
      filter(longitude >= -5 & longitude <= 10 & 
               latitude >= 42 & latitude <= 51)
    
    # Retourner les données filtrées
    data_filtered %>%
      select(longitude, latitude, etiquette_dpe, surface_habitable_logement, type_batiment, everything())
  })
  
 
  output$Répartition_surface_maison_73 <- renderPlot({
    
    df_maison = df_total[df_total$type_batiment == "maison",]
    df_maison = df_maison[df_maison$surface_habitable_logement <=350,] 
    df_maison_73 = df_maison[grepl("^73", df_maison$code_insee_ban), ]
    
    ggplot(df_maison_73, aes(x = surface_habitable_logement)) +
      geom_histogram(
        breaks = seq(0, 350, length.out = 8), 
        fill = "red", 
        color = "black"
      ) +
      scale_x_continuous(limits = c(0, 350)) +
      scale_y_continuous(limits = c(0, 2000)) +
      labs(
        title = "Répartition des surfaces habitables des maisons du 73",
        x = "Surface habitable",
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
    
    df_maison = df_total[df_total$type_batiment == "maison",]
    df_maison = df_maison[df_maison$surface_habitable_logement <=350,] 
    df_maison_74 = df_maison[grepl("^74", df_maison$code_insee_ban), ]
    
    ggplot(df_maison_74, aes(x = surface_habitable_logement)) +
      geom_histogram(
        breaks = seq(0, 350, length.out = 8), 
        fill = "red", 
        color = "black"
      ) +
      scale_x_continuous(limits = c(0, 350)) +
      scale_y_continuous(limits = c(0, 2000)) +
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
  
  output$dpe_map <- renderLeaflet({
    data <- map_data()
    
    # Si le data frame est vide après le filtrage/conversion, affichez une carte centrée
    if(nrow(data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = 6.4, lat = 45.7, zoom = 9) %>%
               addPopups(lng = 6.4, lat = 45.7, 
                         popup = "Aucune donnée de localisation valide à afficher (vérifiez les coordonnées ou le format du fichier)."))
    }
    
    # Définir les couleurs DPE pour la visualisation
    dpe_colors <- c("A" = "#008000", "B" = "#339900", "C" = "#66B200", 
                    "D" = "#FFCC00", "E" = "#FF9933", "F" = "#FF6666", "G" = "#CC0000")
    
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
        fillColor = ~dpe_colors[etiquette_dpe], 
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.7,
        clusterOptions = markerClusterOptions() # Regroupement des marqueurs
      )
  })
}

# --- Exécution de l'application ---
shinyApp(ui = ui, server = server)
