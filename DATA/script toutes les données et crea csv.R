#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
# Paramètres de la requête

df_total <- data.frame()

params <- list(
  page = 1,
  size = 10000,
  select = "etiquette_dpe,etiquette_ges,date_reception_dpe,type_batiment,type_installation_chauffage,type_installation_ecs,hauteur_sous_plafond,surface_habitable_logement,code_insee_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban,nom_commune_ban,categorie_enr",
  qs = 'code_departement_ban:"74"'
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

# création du dataset final

df_total <- df

base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe02neuf/lines"
# Paramètres de la requête

params <- list(
  page = 1,
  size = 10000,
  select = "etiquette_dpe,etiquette_ges,date_reception_dpe,type_batiment,type_installation_chauffage,type_installation_ecs,hauteur_sous_plafond,surface_habitable_logement,code_insee_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban,nom_commune_ban,categorie_enr",
  qs = 'code_departement_ban:"74"'
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

# ajout de des données au dataset final
df_total <- rbind(df, df_total)

base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
# Paramètres de la requête

params <- list(
  page = 1,
  size = 10000,
  select = "etiquette_dpe,etiquette_ges,date_reception_dpe,type_batiment,type_installation_chauffage,type_installation_ecs,hauteur_sous_plafond,surface_habitable_logement,code_insee_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban,nom_commune_ban,categorie_enr",
  qs = 'code_departement_ban:"73"'
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

# ajout de des données au dataset final
df_total <- rbind(df, df_total)

base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe02neuf/lines"
# Paramètres de la requête

params <- list(
  page = 1,
  size = 10000,
  select = "etiquette_dpe,etiquette_ges,date_reception_dpe,type_batiment,type_installation_chauffage,type_installation_ecs,hauteur_sous_plafond,surface_habitable_logement,code_insee_ban,coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban,nom_commune_ban,categorie_enr",
  qs = 'code_departement_ban:"73"'
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

# ajout de des données au dataset final
df_total <- rbind(df, df_total)

#création du .csv
write.csv2(
  x = df_total, 
  file = "données_projet_DPE.csv",
  row.names = FALSE,       
  fileEncoding = "UTF-8"   
)

