## code to prepare `chelem_classification` dataset goes here
# Importer la classification CHELEM géographique
df_chelem_geo_classification <-
  here::here("data-raw", "CHELEM_classification_geo_2022_02b.xlsx") |>
  readxl::read_xlsx(sheet = "CHELEM geo-classification") |>
  janitor::clean_names() |>
  dplyr::rename(iso = code_a)


# Créer un dataframe dans lequel chaque région est définie
# Pour chaque région on a les pays/sous-régions qui la compose
list_regions <-
  df_chelem_geo_classification |>
  # Garder que les lignes dont le niveau d'aggrégation est de 2
  # Forte aggrégation mais plus faible que le niveau 1 qui aggrège tout
  # Contient les régions ainsi que le détail des pays composants ces régions
  dplyr::filter(!is.na(composition), level %in% 2) |>
  dplyr::select(iso, name, composition) |>
  # Séparer les composants : chaque composanst est séparé par "+"
  # On veut une ligne par composant indiquant à quelle région il appartient
  tidyr::separate_rows(composition, sep = "\\+") |>
  dplyr::rename(
    iso_region = iso,
    name_region = name,
    iso_country = composition
  )

# Créer un dataframe qui répertorie toutes les sous-régions
# Permet de décomposer ces régions en pays
list_sub_regions <-
  df_chelem_geo_classification |>
  # Sélectionner les lignes dont les pays/sous-régions sont présent dans la
  # définition des régions majeures
  # Garder uniquement les sous-régions : ont une composition indiquée
  dplyr::filter(
    iso %in% unique(list_regions$iso_country),
    !is.na(composition)
  ) |>
  dplyr::select(iso, name, composition)

# Fusionner les deux dataframes de régions/sub_regions
# Si iso_country est une région on a sa composition
chelem_classification <-
  list_regions |>
  # Fusionner df regions et sub_regions -> ajoute la composition
  # des sous-regions
  dplyr::full_join(list_sub_regions, by = c("iso_country" = "iso")) |>
  # Chaque composant de sous_region = 1 ligne
  tidyr::separate_rows(composition, sep = "\\+") |>
  # Si composition n'est pas NA alors le iso_country correspond à une
  # sous-region. Il est donc remplacé par les codes pays
  dplyr::mutate(
    iso_country = dplyr::if_else(is.na(composition), iso_country, composition)
  ) |>
  # Garder uniquement les variables d'intérêt : noms et codes des régions et pays
  dplyr::select(!c(name, composition)) |>
  # Certaines composition terminent par "+...". On enlève donc toutes les
  # lignes qui contiennent autre chose que des lettres
  dplyr::filter(grepl("^[[:alpha:]]+$", iso_country))

# Exporter le dataframe en .rda
usethis::use_data(chelem_classification, overwrite = TRUE)
