# app.R
#install.packages(c("ggplot2","leaflet","shiny","dplyr"))
library(ggplot2)
library(shiny)
library(leaflet)
library(dplyr)
df_total <- read.csv2(file = "DATA/données_projet_DPE.csv")

# --- Définition de l'Interface Utilisateur (UI) ---
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Présentation du DPE sur les logements neufs et existants en Savoie et Haute-Savoie"),
  
  # Création des onglets (tabsetPanel)
  tabsetPanel(
    
    # Onglet 1
    tabPanel("Comparaison territoire",
          plotOutput("Répartition_surface_maison_73"),
          plotOutput("Répartition_surface_maison_74")),

    # Onglet 2
    tabPanel("Onglet 2"),
    
    # Onglet 3
    tabPanel("Onglet 3")
    )
  )

server <- function(input, output) {
  output$Répartition_surface_maison_73 <- renderPlot({
    
    df_maison = df_total[df_total$type_batiment == "maison",]
    df_maison = df_maison[df_maison$surface_habitable_logement <=500,]
    df_maison_73 = df_maison[grepl("^73", df_maison$code_insee_ban), ]
    hist(
      df_maison_73$surface_habitable_logement,
      breaks = seq(0, 500, length.out = 11),
      xlim = c(0, 500),
      ylim = c(0, 5000),
      xlab = "Surface habitable",
      ylab = "Effectifs",
      main = "Répartition des surfaces habitables des maisons du 73",
      col = "red"
    )
  })
  
  output$Répartition_surface_maison_74 <- renderPlot({
    
    df_maison = df_total[df_total$type_batiment == "maison",]
    df_maison = df_maison[df_maison$surface_habitable_logement <=500,]
    df_maison_74 = df_maison[grepl("^74", df_maison$code_insee_ban), ]
    hist(
      df_maison_74$surface_habitable_logement,
      breaks = seq(0, 500, length.out = 11),
      xlim = c(0, 500),
      ylim = c(0, 5000),
      xlab = "Surface habitable",
      ylab = "Effectifs",
      main = "Répartition des surfaces habitables des maisons du 74",
      col = "red"
    )
  })
}

# --- Exécution de l'application ---
shinyApp(ui = ui, server = server)