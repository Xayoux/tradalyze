# Documentation -------------------------------------------------------------
## Définition de la fonction -----------------------------------------------
#' @title Download the BACI Database
#'
#' @description `dl_baci` allows you to download the
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' database produced by the [CEPII](http://www.cepii.fr/CEPII/en/welcome.asp).
#' You can select the revision and version of the BACI you require. The zip
#' file will be downloaded and saved to the location of your choice using
#' the `dl_folder` parameter.
#'
#' @details Variables in the BACI database are the following :
#' - \strong{t} : Numeric. Year of the trade flow.
#' - \strong{i} : Numeric. Code iso numeric of the exporter.
#' - \strong{j} : Numeric. Code iso numeric of the importer.
#' - \strong{exporter} : Character. Code iso3 character of the exporter
#' - \strong{importer} : Character. Code iso3 character of the importer.
#' - \strong{k} : Character. Product code HS6.
#' - \strong{v} : Numeric. Value of the trade flow in thousand current dollars.
#' - \strong{q} : Numeric. Quantity in metric tons. 
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
#' @return The BACI database in parquet files. A folder containing the BACI
#' csv files (can be deleted to save space). A folder containing the
#' information files (country and product codes and a README). The zip file
#' (which can be deleted).
#' 
#' 
#' @examples
#' # Download the latest version of the revision 1992 in your current directory
#' # dl_baci(dl_folder = "folder-of-the-download")
#'
#' # Download the 202401 version of BACI for the 2007 revision
#' # dl_baci(version = "202401", revision = "HS07", dl_folder = "folder-of-the-download")
#'
#' @source [Gaulier, G. et Zignago, S. (2010) BACI: International Trade Database at the Product-Level. The 1994-2007 Version. CEPII Working Paper, N°2010-23.](http://www.cepii.fr/CEPII/fr/publications/wp/abstract.asp?NoDoc=2726)
#'
#' @export


# Fonction dl_baci ----------------------------------------------------------
dl_baci <- function(version = NULL, revision = "HS92",
                    dl_folder, download = TRUE, unzip = TRUE, to_parquet = TRUE,
                    rm_zip = FALSE, rm_csv = FALSE){

  ## Checking validity of parameters ----------------------------------------
  # Check if version is NULL or a string
  if (!is.null(version) & !is.character(version)){
    stop("version must be NULL or a string.")
  }

  # Check if revision is a character
  if (!is.character(revision)){
    stop("revision is not a string. Keep in mind that it must be starting with \"HS\" followed by a two digit number such as \"HS92\".")
  }
  # If it's a character, check if it's beggin with 'HS'
  else if (!startsWith(revision, "HS")) {
    stop("revision must be a string starting with \"HS\" followed by a two digit number such as \"HS92\".")
  }
  
  # Check if dl_folder is a string
  if (!is.character(dl_folder)){
    stop("dl_folder must be a string specifying the path to a folder.")
  }

  # Check if download is a logical
  if(!is.logical(download)){
    stop("download must be a logical.")
  }

  # Check if unzip is a logical
  if(!is.logical(unzip)){
    stop("unzip must be a logical.")
  }

  # Check if to_parquet is a logical
  if(!is.logical(to_parquet)){
    stop("to_parquet must be a logical.")
  }

  # Check if rm_zip is a logical
  if(!is.logical(rm_zip)){
    stop("rm_zip must be a logical.")
  }

  # Check if rm_csv is a logical
  if(!is.logical(rm_csv)){
    stop("rm_csv must be a logical.")
  }
  
 
  
  ## Download BACI ----------------------------------------------------------
  # Check whether rvest is installed (mandatory for this part of the function)
  rlang::check_installed("rvest", reason = "Mandatory to obtain the download link")
  
  # Load the HTML code of the BACI webpage
  html_baci <-
    rvest::read_html(
      "https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=37"
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
    # The first link for this revision is the latest version
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

    # Keep the link corresponding to the version and revision wished
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

  # If download is set to TRUE (by the user or if the zip file doesn't exist)
  # Then download the zip file
  if (download == TRUE){
    # Check whether svDialogs is installed (mandatory for this part of the function)
    rlang::check_installed("svDialogs", reason = "Mandatory to ask the confirmation of the download")
    # Ask wheter the user whish to dl this version and revision of BACI
    question_dl <- stringr::str_glue("Do you want to download the revision {revision} of BACI V{version} ?")
    response_dl <- svDialogs::dlg_message(question_dl, "yesnocancel")$res

    # If the answer of the user is yes, then download the zip file, else just message
    if (response_dl == "yes"){
      # Launch the download
      curl::multi_download(
        dl_zip_link,
        path_zip_file
      )
    }
    # The user don't want to DL the zip file
    else {
      # Tell the user that he doesn't want to download BACI
      message(stringr::str_glue("The user don't want to download the revision {revision} of BACI V{version}"))

      # If the zip file does not exist and there is no DL : stop the process
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
    message(stringr::str_glue("\n\"{path_zip_file}\" already exists. There is no download to perform.\n"))
  }

  ## Unzip process ----------------------------------------------------------
  # Path for the folder containing the csv files
  path_baci_csv_folder <-
    here::here(
      dl_folder,
      stringr::str_glue("BACI_{revision}_V{version}-csv")
    )

  # Path for the folder containing the parquet files
  path_baci_parquet_folder <-
    here::here(
      dl_folder,
      stringr::str_glue("BACI_{revision}_V{version}-parquet")
    )
  
  # Regex to check if csv files exist for this version and revision of BACI
  regex_csv_files <- as.character(stringr::str_glue("BACI_{revision}_Y\\d{{4}}_V{version}\\.csv"))

  # Look the number of csv files existing for this revision and version
  nb_csv_files <- length(list.files(path_baci_csv_folder, pattern = regex_csv_files))

  # Look the number of parquet files existing for this version anhd revision
  nb_parquet_files <- length(list.files(path_baci_parquet_folder))


  # If unzip is TRUE always unzip. If not unzip only if there is not a single csv or parquet file
  if (unzip == TRUE){
    # Message indicating that the extraction process begin
    message("\nExtract process begin :")

    # Begin the extraction of csv files
    path_zip_file  |>
      utils::unzip(exdir = path_baci_csv_folder)

    # Message that the extraction has ended
    message(stringr::str_glue("\nThe extraction of csv files for the revision {revision} of BACI version {version} has ended.\n"))
  }
  # Il there is not a single csv file and parquet file : unzip
  else if (nb_csv_files == 0 & nb_parquet_files == 0){
    # Message indicating that the extraction process begin
    message(stringr::str_glue("\nThere is no csv or parquet files for the revision {revision} of BACI version {version}.\nExtract process begin :\n"))

    # Unzip the zip file
    path_zip_file  |>
      utils::unzip(exdir = path_baci_csv_folder)

    # Message that the extraction has ended
    message(stringr::str_glue("\nThe extraction of csv files for the revision {revision} of BACI version {version} has ended.\n"))
  }
  # If there is at least one csv file or parquet file : dont unzip 
  else {
    # Message that there will be no extraction
    message(stringr::str_glue("\nThere is already some csv or parquet files for the revision {revision} of BACI version {version}.\nThere is no extract process.\n"))
  }

  ## Transform csv into parquet files --------------------------------------
  # Vector containing path of all BACI csv files
  vector_path_csv_files <-
    path_baci_csv_folder |>
    list.files(pattern = regex_csv_files, full.names = TRUE)

  # Define the importat schema for the BACI database
  # Allows to control for the type of variables
  # All string to avoid any problems with the import
  schema_baci <-
    arrow::schema(
      arrow::Field$create("t", type = arrow::string()),
      arrow::Field$create("i", type = arrow::string()),
      arrow::Field$create("j", type = arrow::string()),
      arrow::Field$create("k", type = arrow::string()), # Avoid the disparition of 0
      arrow::Field$create("v", type = arrow::string()), 
      arrow::Field$create("q", type = arrow::string()) 
    )

  message(stringr::str_glue("\nThe folder \"{path_baci_parquet_folder}\" has been created.\n"))

  # if to_parquet is TRUE or if there is not a single one parquet file for this version and revision :
  # convert csv files to parquet files
  if (to_parquet == TRUE){
    # Delete folder of parquet files to avoid confusion between older and newer files.
    unlink(path_baci_parquet_folder, recursive = TRUE)

    # message indicating that the folder of paruqet files will be deleted
    # Avoid any confusion between old and new files
    message(stringr::str_glue("\nThe folder \"{path_baci_parquet_folder}\" has been deleted to avoid confusion between older and newer files.\n"))

    # Check if readr is installed to read the country-code csv
    rlang::check_installed("readr", reason = "\n\nNecessary to read the csv of the country codes.")

    # Message indicating the beginning of the convert process
    message("\nBeginning of the convert process :\n")
    
    # Import the file containing the country codes
    # ALlows to match countries name and their iso cope numeric
    df_country_codes <-
      here::here(path_baci_csv_folder, stringr::str_glue("country_codes_V{version}.csv")) |>
      readr::read_csv(show_col_types = FALSE) |>
      dplyr::select(country_code, country_iso3)|>
      # Change the type of country_code to string to match thje import of BACI
      dplyr::mutate(country_code = as.character(country_code))
    
    # Load, mutate and write baci database
    vector_path_csv_files |>
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
      # Convert variables that must be numeric : remove all spaces and
      # if there is letter : take NA value
      dplyr::mutate(
        t = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(t), "^[0-9.]+$"), NA, stringr::str_trim(t))),
        i = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(i), "^[0-9.]+$"), NA, stringr::str_trim(i))),
        j = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(j), "^[0-9.]+$"), NA, stringr::str_trim(j))),
        q = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(q), "^[0-9.]+$"), NA, stringr::str_trim(q))),
        v = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(v), "^[0-9.]+$"), NA, stringr::str_trim(v)))
      ) |>
      # Group by years for the writing
      dplyr::group_by(t) |>
      arrow::write_dataset(
        path = path_baci_parquet_folder,
        format = "parquet"
      )

    # Message indicating the end of the convert process
    message("\nEnd of the convert process.")

    # Copy info files in an info folder
    # file to be copy
    files_to_copy <-
      c(
        "country_codes_V202401b.csv",
        "product_codes_HS92_V202401b.csv",
        "Readme.txt"
      )

    # Recreate the path of the files 
    path_files_to_copy <- file.path(path_baci_csv_folder, files_to_copy)

    # Create the dir for the info files
    dir.create(here::here(dl_folder, stringr::str_glue("BACI_{version}_V{revision}-infos")))
    
    # Copy files
    file.copy(
      from = path_files_to_copy,
      to = here::here(dl_folder, stringr::str_glue("BACI_{version}_V{revision}-infos")),
      overwrite = TRUE
    )
  }
  # If to_parquet = FALSE and there is not a single one parquet file for this version and revision : 
  else if(length(list.files(path_baci_parquet_folder)) == 0){
    # Check if readr is installed to read the country-code csv
    rlang::check_installed("readr", reason = "\n\nNecessary to read the csv of the country codes.")
    
    # Message indicating the beginning of the convert process
    message(stringr::str_glue("\nThere is no parquet files for the revision {revision} of BACI version {version}.\nConvert process begin :\n"))

    # Import the file containing the country codes
    # Allows to match countries name and their iso cope numeric
    df_country_codes <-
      here::here(path_baci_csv_folder, stringr::str_glue("country_codes_V{version}.csv")) |>
      readr::read_csv(show_col_types = FALSE) |>
      dplyr::select(country_code, country_iso3)|>
      dplyr::mutate(country_code = as.character(country_code))

    # Load, mutate and write baci database
    vector_path_csv_files |>
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
      dplyr::group_by(t) |>
      arrow::write_dataset(
        path = path_baci_parquet_folder,
        format = "parquet"
      )

    # Message indicating the end of the convert process
    message("\nEnd of the convert process.")

    # Copy info files in an info folder
    # file to be copy
    files_to_copy <-
      c(
        stringr::str_glue("country_codes_V{version}.csv"),
        stringr::str_glue("product_codes_{revision}_V{version}.csv"),
        "Readme.txt"
      )

    # Recreate the path of the files 
    path_files_to_copy <- file.path(path_baci_csv_folder, files_to_copy)

     # Create the dir for the info files
    dir.create(here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}-infos")))

    # Copy files
    file.copy(
      from = path_files_to_copy,
      to = here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}-infos")),
      overwrite = TRUE
    )
  }
  # if there is already some parquet files for this version and revision : do nothing
  else {
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


