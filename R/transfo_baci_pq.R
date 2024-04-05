#' Transforme des fichiers csv de BACI en format parquet
#'
#' @param csv_folder Chemin d'accès au dossier contenant les fichiers csv de BACI
#' @param path_output Chemin d'accès au dossier où seront stockés les fichiers parquet de BACI (par défaut, le même que csv_folder).
#' Ils seront stockés dans un dossier nommé "BACI-parquet".
#' @param version Version des fichiers BACI. Trouvable sur la page [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#'
#' @return Les fichiers parquet de BACI.
#' @export
#'
#' @examples # Pas d'exemple.
transfo_baci_pq <- function(csv_folder, path_output = csv_folder, version){

  # Définition des messages d'erreur ----------------------------------------
  # Message d'erreur si path_baci_parquet n'est pas une chaine de caractère
  if(!is.character(csv_folder)){
    stop("csv_folder doit \uEAtre un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
  }

  # Message d'erreur si path_output n'est pas une chaine de caractère
  if(!is.character(path_output)){
    stop("path_output doit \uEAtre un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
  }

  baci_path_vector <-
    csv_folder |>
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
    here::here(csv_folder, stringr::str_glue("country_codes_V{version}.csv")) |>
    readr::read_csv() |>
    dplyr::select(country_code, country_iso3) |>
    dplyr::mutate(country_code = as.character(country_code))

  # Créer le dossier BACI-parquet s'il n'existe pas
  if (!dir.exists(here::here(path_output, "BACI-parquet"))) {
    dir.create(here::here(path_output, "BACI-parquet"), recursive = TRUE)
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
      path = here::here(path_output, "BACI-parquet"),
      format = "parquet"
    )

}
