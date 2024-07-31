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
#' @param download A logical that indicates whether the zip file must be
#' downloaded even if it already exist. If FALSE, the zip file will only be
#' downloaded if it does not exist.
#' @param unzip A logical indicating whether the zip file must be unpacked even
#' if csv or parquet files already exist. If FALSE, the zip file will only be
#' unpacked if there is not a single csv and parquet file with the same
#' revision and version you want in the csv and parquet folder.
#' @param to_parquet A logical indicating whether the csv files must be
#' transform into parquet files even if parquet files already exist. If FALSE,
#' the csv files will only be transformed into parquet files if there is not a
#' single one parquet file with the same revision and version you want in the
#' parquet folder.
#' @param rm_zip A logical indicating whether or not the zip file must be
#' deleted at the end of the process. The default is to delete the zip file.
#' @param rm_csv A logical specifying whether the csv folder and all its files
#' should be deleted at the end of the process. The csv folder is deleted
#' by default. 
#' 
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
                    dl_folder, download = TRUE, unzip = TRUE, to_parquet = TRUE,
                    rm_zip = FALSE, rm_csv = TRUE){

  ## Checking validity of parameters ----------------------------------------
  # Check whether rvest and svDialogs are installed (mandatory for this fonction)
  rlang::check_installed("rvest", reason = "Mandatory to obtain the download link")
  rlang::check_installed("svDialogs", reason = "Mandatory to ask the confirmation of the download")

  # Check if version is NULL or a string
  if (is.null(version) == FALSE & is.character(version) == FALSE){
    stop("version must be NULL or a string.")
  }

  # Check if revision is a character
  if (is.character(revision) == FALSE){
    stop("revision is not a string. Keep in mind that it must be starting with \"HS\" followed by a two digit number such as \"HS92\".")
  }
  # If it's a character, check if it's beggin with 'HS'
  else if (startsWith(revision, "HS") == FALSE) {
    stop("revision must be a string starting with \"HS\" followed by a two digit number such as \"HS92\".")
  }
  
  # Check if dl_folder is a string
  if (is.character(dl_folder) == FALSE){
    stop("dl_folder must be a string specifying the path to a folder.")
  }

  # Create the dl_folder if it doesn't exist already
  if (!file.exists(dl_folder)) {
    dir.create(dl_folder, showWarnings = FALSE, recursive = TRUE)
    message(stringr::str_glue("The folder : \"{dl_folder}\" has been created."))
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
    regex_version_revision <- stringr::str_glue("({revision}.*V{version}.zip)") 
    
    dl_zip_link <-
      vector_dl_zip_links  %>%
      grep(regex_version_revision, ., value = TRUE)

    # Stop the process if no link is found
    if(is.na(dl_zip_link)){
      stop("The revision or version you requested is not available or is incorrect. Please select another.")
    }
  }

  # Name of the zip file 
  zip_name <- stringr::str_glue("BACI_{revision}_V{version}.zip")

  # Path to the zip file
  path_zip_file <-
    here::here(
      dl_folder,
      zip_name
    )

  ## Download process -------------------------------------------------------

  # If parameter download is set to FALSE : check if the zip file already exist
  if (download == FALSE){
    # If it exists, leave the download set to FALSE : the DL is not going to happen.
    download <- dplyr::if_else(file.exists(path_zip_file), FALSE, TRUE)
  }

  # If download is set to TRUE (by the user of if the zip file doesn't exist)
  # Then download the zip file
  if (download == TRUE){
    # Ask wheter the user whish to dl this version and revision of BACI
    question_dl <- stringr::str_glue("Do you want to download the revision {revision} of BACI V{version} ?")
    response_dl <- svDialogs::dlg_message(question_dl, "yesnocancel")$res

    # If the answer of the user is yes, then download the zip file, else just message
    if (response_dl == "yes"){
      curl::multi_download(
        dl_zip_link,
        path_zip_file
      )
    }
    # The user don't want to DL the zip file
    else {
      # Tell the user that he doesn't want to download BACI
      message(stringr::str_glue("The user don't want to download the revision {revision} of BACI V{version}"))

      # If tje zip file does not exist and there is no DL : stop the process
      if (!file.exists(path_zip_file)){
        stop_message <-
          stringr::str_glue("The file \"{path_zip_file}\" does not exist and the user does not wish to download it.\nEnd of process.\n")
        return(message(stop_message))
      }
    }
  }
  # If the file already exist and the user prefer to not Dl it if it exist
  else {
    # Tell the user that there is no need to dl BACI because the zip file already exists
    message(stringr::str_glue("\"{path_zip_file}\" already exists. There is no download to perform.\n"))
  }

  ## Unzip process ----------------------------------------------------------
  path_baci_csv_folder <-
    here::here(
      dl_folder,
      stringr::str_glue("BACI_{revision}_V{version}-csv")
    )
  
  path_baci_parquet_folder <-
    here::here(
      dl_folder,
      stringr::str_glue("BACI_{revision}_V{version}-parquet")
    )
  
  # Regex to check if csv files exist for this version and revision of BACI
  regex_csv_files <- as.character(stringr::str_glue("BACI_{revision}_Y\\d{{4}}_V{version}\\.csv"))
  nb_csv_files <- length(list.files(path_baci_csv_folder, pattern = regex_csv_files))
  nb_parquet_files <- length(list.files(path_baci_parquet_folder))


  # If unzip is TRUE always unzip. If not unzip only if there is not a single csv file
  if (unzip == TRUE){
    message("Extract process")
    path_zip_file  |>
      utils::unzip(exdir = path_baci_csv_folder)
  }
  # Il there is not a single csv file and parquet file : unzip
  else if (nb_csv_files == 0 & nb_parquet_files == 0){
    message(stringr::str_glue("There is no csv files for the revision {revision} of BACI version {version}.\nExtract process\n"))

    path_zip_file  |>
      utils::unzip(exdir = path_baci_csv_folder)
  }
  # If there is at least one csv file or parquet file : dont unzip 
  else {
    message(stringr::str_glue("There is already some csv or parquet files for the revision {revision} of BACI version {version}.\nThere is no extract process\n"))
  }

  ## Transform csv into parquet files --------------------------------------
  vector_path_csv_files <-
    path_baci_csv_folder |>
    list.files(pattern = regex_csv_files, full.names = TRUE)

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

  # Créer le dossier BACI-parquet s'il n'existe pas  
  if (!dir.exists(path_baci_parquet_folder)) {
    dir.create(path_baci_parquet_folder, recursive = TRUE)
  }

  if (to_parquet == TRUE){
    unlink(path_baci_parquet_folder, recursive = TRUE)
    
    df_country_codes <-
      here::here(path_baci_csv_folder, stringr::str_glue("country_codes_V{version}.csv")) |>
      readr::read_csv(show_col_types = FALSE) |>
      dplyr::select(country_code, country_iso3)|>
      dplyr::mutate(country_code = as.character(country_code))
    
    # Ecrire la base BACI en parquet : un par année : gain de place + efficacité
    vector_path_csv_files |>
      # Ouvrir BACI : pas en mémoire
      arrow::open_dataset(format = "csv", schema = schema_baci) |>
      ## dplyr::filter(t != "t") |>
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
        path = path_baci_parquet_folder,
        format = "parquet"
      )
    
  } else if(length(list.files(path_baci_parquet_folder)) == 0){
    message(stringr::str_glue("There is no parquet files for the revision {revision} of BACI version {version}.\nConvert process\n"))

    df_country_codes <-
      here::here(path_baci_csv_folder, stringr::str_glue("country_codes_V{version}.csv")) |>
      readr::read_csv(show_col_types = FALSE) |>
      dplyr::select(country_code, country_iso3)|>
      dplyr::mutate(country_code = as.character(country_code))

    
    vector_path_csv_files |>
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
        path = path_baci_parquet_folder,
        format = "parquet"
      )

  } else {
     message(stringr::str_glue("There is already some parquet files for the revision {revision} of BACI version {version}.\nThere is no convert process\n"))
  }

  ## Cleaning -------------------------------------------------------------
  if (rm_csv == TRUE){
    unlink(path_baci_csv_folder, recursive = TRUE)
    message(stringr::str_glue("\"{path_baci_csv_folder}\" and its files had been removed"))
  }

  if (rm_zip == TRUE){
    unlink(path_zip_file, recursive = TRUE)
     message(stringr::str_glue("\"{path_zip_file}\" and its files had been removed"))
  }
  
}



## here::here("..", "BACI_HS92_V202401b-parquet") |>
##   arrow::open_dataset()

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
##     dplyr::mutate(country_code = as.character(counatry_code))

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
