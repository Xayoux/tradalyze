#' Fonction qui permet de télécharger BACI dans la nomenclature voulue (uniquement dernière version disponible). Les données sont ensuite transformées en fichier parquet (1 par année).
#'
#' @param revision Une chaîne de caractères qui indique la nomenclature voulue (HS92 par défaut). Valeurs possibles : HS92, HS96, HS02, HS07, HS12, HS17, HS22.
#' @param dl_folder Un chemin d'accès vers le dossier où les données seront téléchargées.
#' @param rm_csv Un booléen qui indique si les fichiers csv de BACI doivent être supprimés après la transformation en parquet (TRUE par défaut).
#'
#' @return Un dossier parquet par année.
#' @export
#'
#' @examples # Pas d'exemple.
dl_baci <- function(revision = "HS92", dl_folder, rm_csv = TRUE){

  # Messages d'erreurs + avertissements -------------------------------------
  # Créer le dossier s'il n'existe pas + message d'avertissement
  if (!dir.exists(dl_folder)) {
    dir.create(dl_folder, recursive = TRUE)
    message("Le dossier ", dl_folder, " a été créé.")
  }

  # Erreur si revision n'est pas une des valeurs possibles
  if (!revision %in% c("HS92", "HS96", "HS02", "HS07", "HS12", "HS17", "HS22")) {
    stop("La valeur de revision doit être une des suivantes : HS92, HS96, HS02, HS07, HS12, HS17, HS22.")
  }

  # Information sur BACI ----------------------------------------------------
  # Lien vers la page BACI du cepii
  html_baci <- rvest::read_html("http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37")

  # Récupérer la dernière version de BACI
  version <-
    html_baci |>
    rvest::html_nodes(xpath = '//*[@id="telechargement"]') |>
    rvest::html_elements("em") |>
    rvest::html_text()

  # Créer le lien pour télécharger la dernière version de BACI
  dl_link <- stringr::str_glue("http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_{revision}_V{version}.zip")

  # # Tester si le fichier zip de BACI existe déjà
  if (!file.exists(here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")))) {
    # Si le fichier zip n'existe pas, télécharger BACI
    curl::multi_download(
      dl_link,
      here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip"))
    )
  }

  # Décompresser le fichier zip au même endroit
  here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")) |>
    utils::unzip(exdir = dl_folder)

  # Récupérer tous les chemins des fichiers BACI csv
  baci_path_vector <-
    dl_folder |>
    list.files(full.names = TRUE, pattern = "^BACI.*csv$")

  # D2finir un schéma d'importation pour les données BACI
  schema_baci <-
    arrow::schema(
      arrow::Field$create("t", type = arrow::string()),
      arrow::Field$create("i", type = arrow::string()),
      arrow::Field$create("j", type = arrow::string()),
      arrow::Field$create("k", type = arrow::string()), # Evite la disparition de 0
      arrow::Field$create("v", type = arrow::string()), # être sur qu'il n'y a pas de pb
      arrow::Field$create("q", type = arrow::string()) # être sur qu'il n'y a pas de pb
    )

  df_country_codes <-
    here::here(dl_folder, stringr::str_glue("country_codes_V{version}.csv")) |>
    readr::read_csv() |>
    dplyr::select(country_code, country_iso3) |>
    dplyr::mutate(country_code = as.character(country_code))

  # Créer le dossier BACI-parquet s'il n'existe pas
  if (!dir.exists(here::here(dl_folder, "BACI-parquet"))) {
    dir.create(here::here(dl_folder, "BACI-parquet"), recursive = TRUE)
  }

  # Ecrire la base BACI en parquet : un par année : gain de place + efficacité
  baci_path_vector |>
    # Ouvrir BACI : pas en mémoire
    arrow::open_dataset(format = "csv", schema = schema_baci) |>
    dplyr::filter(t != "t") |>
    dplyr::left_join(
      df_country_codes,
      by = c("i" = "country_code")
    ) |>
    dplyr::rename(exporter = country_iso3) |>
    dplyr::left_join(
      df_country_codes,
      by = c("j" = "country_code")
    ) |>
    dplyr::rename(importer = country_iso3) |>
    dplyr::mutate(
      t = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(t), "^[0-9.]+$"), NA, stringr::str_trim(t))),
      i = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(i), "^[0-9.]+$"), NA, stringr::str_trim(i))),
      j = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(j), "^[0-9.]+$"), NA, stringr::str_trim(j))),
      q = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(q), "^[0-9.]+$"), NA, stringr::str_trim(q))),
      v = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(v), "^[0-9.]+$"), NA, stringr::str_trim(v)))
    ) |>
    # Grouper par année (un fichier parquet par année)
    dplyr::group_by(t) |>
    # Ecrire cette base en parquet
    arrow::write_dataset(
      path = here::here(dl_folder, "BACI-parquet"),
      format = "parquet"
    )

  # Supprimer les fichiers csv de BACI pour gain de place
  if (rm_csv == TRUE) {
    dl_folder |>
      list.files(full.names = TRUE, pattern = "^BACI.*csv") |>
      purrr::walk(file.remove)
  }
}
