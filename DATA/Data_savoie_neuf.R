install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

base_url <- "https://data.ademe.fr/datasets/dpe02neuf/api-doc?operation=readLines"
# Paramètres de la requête
annee = c(2021,2022,2023,2024,2025)
mois = c(01,02,03,04,05,06,07,08,09,10,11,12)

for (i in annee)
  for(j in mois)
params <- list(
  page = 1,
  size = 10000,
  select = "etiquette_dpe,etiquette_ges,date_reception_dpe,annee_construction,type_batiment,type_installation_chauffage,type_installation_ecs,hauteur_sous_plafond,surface_habitable_logement,code_insee_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban,nom_commune_ban,besoin_ecs_logement,conso_refroidissement_annuel,categorie_enr",
  qs = 'code_departement_ban:("73" or "74") AND date_reception_dpe:[]'
) 

# Encodage des paramètres
url_encoded <- modify_url(base_url, query = params)
print(url_encoded)

# Effectuer la requête
response <- GET(url_encoded)

# Afficher le statut de la réponse
print(status_code(response))

# On convertit le contenu brut (octets) en une chaîne de caractères (texte). Cela permet de transformer les données reçues de l'API, qui sont généralement au format JSON, en une chaîne lisible par R
content = fromJSON(rawToChar(response$content), flatten = FALSE)

# Afficher le nombre total de ligne dans la base de données
print(content$total)

# Afficher les données récupérées
df <- content$result
dim(df)
View(df)