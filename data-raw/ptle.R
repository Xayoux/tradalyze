## code to prepare `ptle` dataset goes here
# Source : http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=35

# Lire la page html de la base pour télécharger les données à la source
ptle_html_link <-
  rvest::read_html("http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=35")

# Récupérer le lien de téléchargement des données : 1 seul lien avec ProTEE
dl_link <-
  ptle_html_link |>
  rvest::html_nodes("a") |>
  rvest::html_attr("href") |>
  (\(links) grep("ProTEE", links, value = TRUE, ignore.case = TRUE))()

# Récupérer le nom du fichier
ptle_name <-
  stringr::str_extract(dl_link, "([^/]+$)")

# Télécharger les données
curl::multi_download(
  paste0("http://www.cepii.fr", dl_link), # Ajouter le début du lien qui manque
  here::here("data-raw", ptle_name)
)

# Lire les données
ptle <- readr::read_csv(here::here("data-raw", ptle_name))

# Exporter le dataframe en .rda
usethis::use_data(ptle, overwrite = TRUE)
