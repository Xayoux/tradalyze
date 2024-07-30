# Documentation -------------------------------------------------------------
## Définition de la fonction -----------------------------------------------
#' @title Download the BACI Database
#'
#' @description dl_baci' allows you to download the
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' produced by the [CEPII](http://www.cepii.fr/CEPII/en/welcome.asp). You can
#' select the revision and version of the BACI you require. The zip file will
#' be downloaded and saved to the location of your choice using the 'dl_folder'
#' parameter.
#'
#' @param version A string indicating the version of BACI you wish to download.
#' By default it is set to NULL, which means that the latest available version
#' will be used.
#' @param revision A string indicating the revision of the Harmonised System in
#' which you want BACI. It must start with "HS" followed by a two digit number.
#' The default is "HS92", the oldest revision available.
#' @param dl_folder A string that specifies the folder in which you would like
#' to download the zip file of BACI. 
#' @return The BACI database in a zip file in the folder of your choice.
#' 
#' @examples
#' # Download the latest version of the revision 1992 in your current directory
#' # dl_baci(dl_folder = here::here())
#'
#' # Download the 202401 version of BACI for the 2007 revision
#' # dl_baci(version = "202401", revision = "HS07", dl_folder = here::here())
#'
#' @source [Gaulier, G. et Zignago, S. (2010) BACI: International Trade Database at the Product-Level. The 1994-2007 Version. CEPII Working Paper, N°2010-23.](http://www.cepii.fr/CEPII/fr/publications/wp/abstract.asp?NoDoc=2726)
#'
#' @export


# Fonction dl_baci ----------------------------------------------------------
dl_baci <- function(version = NULL, revision = "HS92",
                    dl_folder){

  ## Checking validity of parameters ----------------------------------------
  # Check whether rvest and svDialogs are installed (mandatory for this fonction)
  rlang::check_installed("rvest", reason = "Mandatory to obtain the download link")
  rlang::check_installed("svDialogs", reason = "Mandatory to ask the confirmation of the download")

  # Check if version is NULL or a string
  if (is.null(version) == FALSE & is.character(version) == FALSE){
    stop("version must be NULL or a string.")
  }

  # Check if revision is a string beginning by "HS"
  if (is.character(revision) == FALSE && startsWith(revision, "HS") == FALSE){
    stop("revision must be a string starting with \"HS\" followed by a two digit number such as \"HS92\".")
  }
  
  # Check if dl_folder is a string
  if (is.character(dl_folder) == FALSE){
    stop("dl_folder must be a string specifying the path to a folder.")
  }

  # Create the dl_folder if it doesn't exist already
  if (!file.exists(dl_folder)) {
    dir.create(dl_folder, showWarnings = FALSE, recursive = TRUE)
    message(glue::glue("The folder : \"{dl_folder}\" has been created."))
}
  


  
  ## Download BACI ----------------------------------------------------------
  # Load the HTML code of the BACI webpage
  html_baci <-
    rvest::read_html(
      "http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=37"
    )

  # Scrap all the links to dl baci .zip
  # All versions and revisions
  vector_dl_zip_links <-
    html_baci  |>
    rvest::html_nodes("li") |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  # Find the dl link for the good version and revision
  # If no version is supply : keep the latest (ie the first link)
  if (is.null(version)){
    dl_zip_link <-
      vector_dl_zip_links  %>%
      grep(revision, ., value = TRUE) %>%
      .[1]

    # Stop the process if no link is found
    if(is.na(dl_zip_link)){
      stop("The revision or version you requested is not available or is incorrect. Please select another.")
    }

    # Extract the latest version available : to inform before the dl start
    version <-
      dl_zip_link |>
      stringr::str_extract("(?<=_V)[^_]+(?=\\.zip)") 
  }
  # If a version is supply keep the good link
  else { 
    # Define a regex to find the link with the good revision and version
    regex_version_revision <- glue::glue("({revision}.*V{version}.zip)") 
    
    dl_zip_link <-
      vector_dl_zip_links  %>%
      grep(regex_version_revision, ., value = TRUE)

    # Stop the process if no link is found
    if(is.na(dl_zip_link)){
      stop("The revision or version you requested is not available or is incorrect. Please select another.")
    }
  }

  # Ask wheter the user whish to dl this version and revision of BACI
  question_dl <- glue::glue("Do you want to download the revision {revision} of BACI V{version} ?")
  response_dl <- svDialogs::dlg_message(question_dl, "yesnocancel")$res

  
  if (response_dl == "yes"){
    curl::multi_download(
      dl_zip_link,
      here::here(dl_folder, glue::glue("BACI_{revision}_V{version}.zip"))
    )
  } else {
    message(glue::glue("The user don't want to download the revision {revision} of BACI V{version}"))
  }
} 



##   if (reponse_dl == "yes"){
##     # Créer le lien pour télécharger la dernière version de BACI
##     dl_link <- stringr::str_glue("http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_{revision}_V{version}.zip")

##     # # Tester si le fichier zip de BACI existe déjà

##     # Si dl_zip == TRUE, télécharger le fichier zip
##     # Télécharge le zip même s'il existe
##     if (dl_zip == TRUE) {
##       curl::multi_download(
##         dl_link,
##         here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip"))
##       )
##     }
##     # Si dl_zip == FALSE, vérifier si le fichier zip existe.
##     # S'il existe alors, on ne télécharge pas. Sinon, on télécharge.
##     else {
##       if (!file.exists(here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")))) {
##         # Si le fichier zip n'existe pas, télécharger BACI
##         curl::multi_download(
##           dl_link,
##           here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip"))
##         )
##       }
##     }

##     # Décompresser le fichier zip au même endroit
##     print("Extraction des fichier csv")
##     here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")) |>
##       utils::unzip(exdir = dl_folder)

##     # Créer les formats parquet pour BACI
##     print("Cr\uE9ation des fichiers parquet")
##     tradalyze::transfo_baci_pq(
##       csv_folder = dl_folder,
##       path_output = dl_folder,
##       version = version
##     )

##     # Supprimer les fichiers csv de BACI pour gain de place si rm_csv == TRUE
##     if (rm_csv == TRUE) {
##       dl_folder |>
##         list.files(full.names = TRUE, pattern = "^BACI.*csv") |>
##         purrr::walk(file.remove)
##     }
##     print("Donn\u00E9es de BACI t\u00E9l\uE900charg\u00E9es !")
##   } else {
##     print(stringr::str_glue("Refus de t\u00E9l\u00E9charger BACI {version}"))
##   }



## # Fonction pour transformer un dossier baci en format parquet
## #' Transforme des fichiers csv de BACI en format parquet
## #'
## #' @param csv_folder Chemin d'accès au dossier contenant les fichiers csv de BACI
## #' @param path_output Chemin d'accès au dossier où seront stockés les fichiers parquet de BACI (par défaut, le même que csv_folder).
## #' Ils seront stockés dans un dossier nommé "BACI-parquet".
## #' @param version Version des fichiers BACI. Trouvable sur la page [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
## #'
## #' @return Les fichiers parquet de BACI.
## #' @export
## #'
## #' @examples # Pas d'exemple.
## transfo_baci_pq <- function(csv_folder, path_output = csv_folder, version){

##   # Définition des messages d'erreur ----------------------------------------
##   # Message d'erreur si path_baci_parquet n'est pas une chaine de caractère
##   if(!is.character(csv_folder)){
##     stop("csv_folder doit \uEAtre un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
##   }

##   # Message d'erreur si path_output n'est pas une chaine de caractère
##   if(!is.character(path_output)){
##     stop("path_output doit \uEAtre un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
##   }

##   baci_path_vector <-
##     csv_folder |>
##     list.files(full.names = TRUE, pattern = "^BACI.*csv$")

##   # D2finir un schéma d'importation pour les données BACI
##   schema_baci <-
##     arrow::schema(
##       arrow::Field$create("t", type = arrow::string()),
##       arrow::Field$create("i", type = arrow::string()),
##       arrow::Field$create("j", type = arrow::string()),
##       arrow::Field$create("k", type = arrow::string()), # Evite la disparition de 0
##       arrow::Field$create("v", type = arrow::string()), # être sur qu'il n'y a pas de pb
##       arrow::Field$create("q", type = arrow::string()) # être sur qu'il n'y a pas de pb
##     )

##   df_country_codes <-
##     here::here(csv_folder, stringr::str_glue("country_codes_V{version}.csv")) |>
##     readr::read_csv() |>
##     dplyr::select(country_code, country_iso3) |>
##     dplyr::mutate(country_code = as.character(country_code))

##   # Créer le dossier BACI-parquet s'il n'existe pas
##   if (!dir.exists(here::here(path_output, "BACI-parquet"))) {
##     dir.create(here::here(path_output, "BACI-parquet"), recursive = TRUE)
##   }

##   # Ecrire la base BACI en parquet : un par année : gain de place + efficacité
##   baci_path_vector |>
##     # Ouvrir BACI : pas en mémoire
##     arrow::open_dataset(format = "csv", schema = schema_baci) |>
##     dplyr::filter(t != "t") |>
##     dplyr::left_join(
##       df_country_codes,
##       by = c("i" = "country_code")
##     ) |>
##     dplyr::rename(exporter = country_iso3) |>
##     dplyr::left_join(
##       df_country_codes,
##       by = c("j" = "country_code")
##     ) |>
##     dplyr::rename(importer = country_iso3) |>
##     dplyr::mutate(
##       t = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(t), "^[0-9.]+$"), NA, stringr::str_trim(t))),
##       i = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(i), "^[0-9.]+$"), NA, stringr::str_trim(i))),
##       j = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(j), "^[0-9.]+$"), NA, stringr::str_trim(j))),
##       q = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(q), "^[0-9.]+$"), NA, stringr::str_trim(q))),
##       v = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(v), "^[0-9.]+$"), NA, stringr::str_trim(v)))
##     ) |>
##     # Grouper par année (un fichier parquet par année)
##     dplyr::group_by(t) |>
##     # Ecrire cette base en parquet
##     arrow::write_dataset(
##       path = here::here(path_output, "BACI-parquet"),
##       format = "parquet"
##     )

## }
