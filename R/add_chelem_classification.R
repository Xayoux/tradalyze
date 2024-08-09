# Documentation -----------------------------------------------------------_-
#' @title
#' Add CHELEM World Region Classificiation to BACI Data
#'
#' @description
#' Merge the database
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' with the world region classification of the database
#' [CHELEM](http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=17).
#' Each exporter and importer is assigned to a geographical region. The merge is
#' made with the \link{chelem_classification} dataframe present in this package.
#' The filtering of the data (if wanted is made with the \link{.filter_baci}
#' function of this package.)
#'
#' @details
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' data are expressed at the exporter-importer level. However, it may be relevant
#' to aggregate countries into geographical regions.
#'
#' The fusion with
#' [CHELEM](http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=17)
#' is made with the variables `exporter` and `importer` which are the character
#' iso code 3 of the countries. To be sure to have these variables, it is
#' recommended to download the database
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' with the \link{dl_baci} function from this package.
#'
#' The regions used are the following :
#' - North America
#' - South America, central America and Caribbean
#' - European Union : supposed to be 26 everytime (Great-Britain not comprise)
#' - CIS : Commonwealth of Independent States
#' - Others in Europe
#' - North Africa
#' - Sub-Sahara Africa
#' - Near and Middle East
#' - North-East Asia
#' - South-East Asia
#' - South Asia and Pacific
#' - Australia and New Zealand
#' - RoW : Rest of the World
#'
#' Countries not included in the CHELEM classification are included in the RoW
#' (Rest of the World) classification. See \link{chelem_classification}
#'
#' @param baci BACI data. Can be a path to a csv file.
#' It can also be a path to a folder containing
#' parquet files. Dataframe and ArrowObject are also accepted. Xlsx files are
#' also accepted, but absolutely not recommended because BACI data are too large
#' and data must be in the first sheet.
#' @inheritParams .filter_baci
#' @inheritParams .export_data
#' 
#' @return BACI data with the CHELEM classification. The following variables are
#' added :
#' \describe{
#'   \item{exporter_iso_region}{Str : Character iso code 3 of the region for the exporter country}
#'   \item{exporter_name_region}{Str : Name of the region for the exporter country}
#'   \item{importer_iso_region}{Str : Character iso code 3 of the region for the importer country}
#'   \item{importer_name_region}{Str : Name of the region for the importer country}
#' }
#'
#' @examples
#' # Add classification and return the data
#' ## add_chelem_classification(
#' ##   baci = "path-to-baci-parquet-folder",
#' ##   return_output = TRUE
#' ## )
#'
#' # Add classification, save the data to parquet format and return arrow format data
#' ## add_chelem_classification(
#' ##   baci = "path-to-baci-parquet-folder",
#' ##   path_output = "path-to-baci-classification-parquet-folder"
#' ##   return_output = TRUE,
#' ##   return_arrow = TRUE
#' ## )
#'
#' # Filter to keep years between 2015 and 2020,aAdd classification,
#' # save the data to csv format and return a tibble
#' ## add_chelem_classification(
#' ##   baci = "path-to-baci-parquet-folder",
#' ##   years = 2015:2020,
#' ##   path_output = "path-to-baci-classificiation-csv-file.csv",
#' ##   return_output = TRUE,
#' ##   return_arrow = FALSE
#' ## )
#'
#' @seealso
#' [.load_data()] for more informations concerning the loading of data
#' [.filter_baci()] for more informations concerning the filtering of data.
#' [.export_data()] for more informations concerning the export of data.
#' 
#' @source [CHELEM classification of the CEPII](<http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=17>).
#' See :
#' [de Saint Vaulry, A. (2008), “Base de données CHELEM - Commerce international du CEPII”,  Document de travail du CEPII, N°2008-09](http://www.cepii.fr/cepii/fr/publications/wp/abstract.asp?nodoc=1081)
#'
#' @export

# Fonction ------------------------------------------------------------------
## Definition ---------------------------------------------------------------
add_chelem_classification <- function(baci, years = NULL, codes = NULL,
                                      export_countries = NULL, import_countries = NULL,
                                      return_output = TRUE, return_arrow = TRUE,
                                      path_output = NULL){

  ## Error messages ----------------------------------------------------------
  # Check validity of export parameters
  tradalyze::.export_data(
    data = NULL,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = FALSE,
    collect = NULL
  )

  ## Add chelem classification to the data -----------------------------------
  # Load the data
  df_baci <- tradalyze::.load_data(baci)

  
  # Filter the data
  df_baci <-
    tradalyze::.filter_baci(
      df_baci = df_baci,
      years = years,
      codes = codes,
      export_countries = export_countries,
      import_countries = import_countries
    )


  # Add the CHELEM classification to addd geographic regions
  df_baci <-
    df_baci |>
    # Associate each exporter to his region
    dplyr::left_join(
      tradalyze::chelem_classification,
      by = c("exporter" = "iso_country")
    ) |>
    # If the exporter is not present in the CHELEM classification
    # Or if there is a matching problem -> goes to the rest of the world (row)
    dplyr::mutate(
      iso_region = dplyr::if_else(is.na(iso_region), "RoW", iso_region),
      name_region = dplyr::if_else(is.na(name_region), "RoW", name_region)
    ) |>
    # Rename regions variables to integrate the exporter dimmension
    dplyr::rename(
      exporter_iso_region = iso_region,
      exporter_name_region = name_region
    ) |>
    # Associate each importer to his region
    dplyr::left_join(
      tradalyze::chelem_classification,
      by = c("importer" = "iso_country")
    ) |>
    # If the importer is not present in the CHELEM classification
    # Or if there is a matching problem -> goes to the rest of the world (row)
    dplyr::mutate(
      iso_region = dplyr::if_else(is.na(iso_region), "RoW", iso_region),
      name_region = dplyr::if_else(is.na(name_region), "RoW", name_region)
    ) |>
    # Rename regions variables to integrate the importer dimmension
    dplyr::rename(
      importer_iso_region = iso_region,
      importer_name_region = name_region
    )

  # Export the data
  tradalyze::.export_data(
    data = df_baci,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = TRUE,
    collect = FALSE
  )
}



