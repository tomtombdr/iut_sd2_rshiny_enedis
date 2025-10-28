# app.R
#install.packages("ggplot2","leaflet")
library(ggplot2)
library(shiny)
library(leaflet)
df_total <- read.csv(file = "DATA/données_projet_DPE.csv")
# --- Définition de l'Interface Utilisateur (UI) ---
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Présentation du DPE sur les logements en France"),
  
  # Création des onglets (tabsetPanel)
  tabsetPanel(
    
    # Onglet 1
    tabPanel("Onglet 1"
             ), 

    # Onglet 2
    tabPanel("Onglet 2"),
    
    # Onglet 3
    tabPanel("Onglet 3")
  )
)


server <- function(input, output) {
}

# --- Exécution de l'application ---
shinyApp(ui = ui, server = server)