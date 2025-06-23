# Utilitary functions ------------------------------------------------------
#' @title Ensure Devtools Installed
#' @description Checks if devtools is installed and installs it if not.
#' @noRd
# This function should not be exported.
ensure_devtools_installed <- function(){
  if (!rlang::is_installed("devtools")){
    utils::install.packages("devtools")
  }
}

#' @title Install Concordance from GitHub
#' @description Installs the concordance package from GitHub.
#' @noRd
# This function should not be exported.
install_concordance_github <- function(){
  ensure_devtools_installed()

  if (rlang::is_installed("concordance")){
    utils::remove.packages("concordance")
  }
  
  devtools::install_github("insongkim/concordance")
}

#' @title Ask to Install Concordance
#' @description Prompts the user to install the appropriate version of the concordance package.
#' @param current_version The version of the concordance package. If NULL the
#' package is not installed.
#' @noRd
# This function should not be exported.
ask_install_concordance <- function(current_version = NULL){
  # If a version is provided : the package concordance is already installed
  # But the version is < 2.1.0
  if (!is.null(current_version)){
    message(glue::glue("Your version of the concordance package is {current_version}."))
    message(glue::glue("To use the latest revision (HS6), you must have a version equal or greater than 2.1.0."))
    message(glue::glue("This version is not available on CRAN and must be downloaded from GitHub."))

    question_message <- "Do you want to download the latest version of concordance from GitHub and the packages needed for that (like devtools)?"

    prompt <- "Y/N/cancel"
  }
  # If there is no version provided the package concordance is not installed
  else {
    question_message <- "You don't have the concordance package installed. Do you want the GitHub version to access the latest revision (HS6), or do you prefer the CRAN version?"

    prompt <- "GitHub/CRAN/cancel"
  }

  # Ask the question to download from github or cran
  # The question depends whether the package concordance is already installed
  # Convert the answer is character for the next step
  response_question <-
    as.character(
      utils::askYesNo(
        msg = question_message,
        prompts = prompt
      )
    )

  # Download concordance from github if wanted
  # Or download from CRAN
  # Or do nothing / cancel the function
  switch(
    response_question,
    "TRUE" = install_concordance_github(),
    "FALSE" = {
      if (is.null(current_version)){
        utils::install.packages("concordance")
      } else {
        message(glue::glue("You are staying with the version {current_version} of the concordance package. The revision HS6 will not be available."))
      }
    },
    "NA" = stop("You chose to cancel. No changes were made.")
  )
}



#' @title Create a Dataframe Usefull for Estimate the Quality of a Product
#'
#' @description
#' This function allow to combine data from
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37),
#' [Gravity](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8)
#' and [Product Level Trade Elasticities](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=35)
#' created by the [CEPII](http://www.cepii.fr/CEPII/en/welcome.asp). This
#' dataframe can be used to estimate the quality of a product
#' from an equation à la Khandelwal et al (2013) (see Bas et al 2015 for an
#' application with aggregate commercial data).
#'
#' @details
#' This feature uses [arrow](https://arrow.apache.org/docs/r/) functionalities.
#' However, the computation of the various metrics must be in memory
#' (only this part is in memory). This can take some time depending on your
#' configuration and the size of your data. If the size of the data is too
#' large for your computer, it may crash. It is advisable to reduce the size
#' of your database and run this function several times.
#'
#' The following variables are mandatory to have in baci data :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{exporter}{Numeric/character : Iso numeric or character of the exporter}
#'   \item{importer}{Numeric/character : Iso numeric or character of the importer}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#' }
#'
#' The following variables are mandatory to have in Gravity data
#' \describe{
#'   \item{iso3_o}{Character : Code iso numeric for the origin country.}
#'   \item{iso3_d}{Character : Code iso numeric for the destination country.}
#'   \item{year}{Numeric : Year.}
#' }
#'
#' And of course all variables you want to have with `gravity_variables`
#' and `baci_variables`.
#'
#' @param gravity Gravity data. Can be a path to a csv file.
#' It can also be a path to a folder containing
#' parquet files. Dataframe and ArrowObject are also accepted. Xlsx files are
#' also accepted, but absolutely not recommended because Gravity data are too large
#' and data must be in the first sheet.
#' @param gravity_variables Variables to keep from gravity in the output dataframe.
#' By default (if NULL) all the variables are beeing kept. No matter the value
#' of this parameter, the following variables are beeing kept :
#' exporter, importer, iso3_o, iso3_d.
#' @param baci_variables Variables to keep from baci in the output dataframe.
#' Bu default (if NULL) all the variables are beeing kept. No matter the value
#' of this parameter, the following variables are beeing kept :
#' exporter, importer, k, t, v et q.
#' @param revision_codes A character indicating the HS revision of the product
#' codes in BACI. By default this is the revision
#' "HS0" (1988/1992). The value can also be : "HS1" (1996), "HS2" (2022), "HS3" (2007),
#' "HS4" (2012) or "HS5" (2017). If the version of the
#' \link{concordance} package is greater than 2.1.0 you can also
#' choose "HS6" (2022).
#' @inheritParams add_chelem_classification
#' @inheritParams .filter_baci
#' @inheritParams .export_data
#' @inheritParams clean_uv_outliers
#'
#' @return Dataframe with the variable beeing kept from baci and gravity and the
#' following variables :
#' \describe{
#'   \item{sigma}{Nuemric : Elasticities of the product taken from the PLTE database.}
#'   \item{p}{Numeric : the "price" (unit value) of the trade flow (p = v / q)}
#'   \item{demand}{Numeric : The value of the demand for this trade flow.
#' \eqn{demand = log(q) + (\sigma_k * log(p))}}
#' }
#'
#' @examples
#' ## create_quality_df(
#' ##   baci = "path-to-folder-baci-parquet",
#' ##   gravity = "path-to-gravity-folder-parquet",
#' ##   years = 2010:2020,
#' ##   gravity_variables = c("dist", "contig", "comlang_off"),
#' ##   revision_codes = "HS0",
#' ##   return_output = TRUE,
#' ##   return_parquet = FALSE,
#' ##   path_output = "path-to-folder-output-parquet"
#' ## )
#'
#' @source [Gaulier, G. and Zignago, S. (2010) BACI: International Trade Database at the Product-Level. The 1994-2007 Version. CEPII Working Paper, N°2010-23](http://www.cepii.fr/PDF_PUB/wp/2010/wp2010-23.pdf)
#' @source [Conte, M., P. Cotterlaz and T. Mayer (2022), "The CEPII Gravity database". CEPII Working Paper N°2022-05, July 2022.](http://www.cepii.fr/DATA_DOWNLOAD/gravity/doc/Gravity_documentation.pdf)
#' @source [Fontagné L., Guimbard H. and Orefice G. (2022) Product-Level Trade Elasticities. Journal of International Economics, vol 137](https://www.sciencedirect.com/science/article/abs/pii/S0022199622000253?via%3Dihub)
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' [.filter_baci()] For more informations concerning the filter of data inside the function.
#' [.export_data()] For more informations concerning the export of the data inside the function.
#' @export
create_quality_df <- function(baci, gravity, years = NULL, codes = NULL,
                              export_countries = NULL, import_countries = NULL,
                              gravity_variables = NULL, baci_variables = NULL,
                              revision_codes = c("HS0", "HS1", "HS2", "HS3", "HS4", "HS5", "HS6"),
                              return_output = FALSE,
                              return_arrow = FALSE, path_output = NULL,
                              na.rm = TRUE){

  # Check if gravity_variables is character or NULL
  tradalyze::.check_null_character(gravity_variables, "gravity_variables")

  # Check if baci_variables is character or NULL
  tradalyze::.check_null_character(baci_variables, "baci_variables")

  # Check validity of revision_codes
  revision_codes <- match.arg(revision_codes)

  # Check validity of export parameters
  tradalyze::.export_data(
    data = NULL,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = FALSE,
    collect = NULL
  )

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Check the version of concordance
  # If concordance is installed check the version
  if (rlang::is_installed("concordance")){
    version_concordance <- utils::packageVersion("concordance")
    # If the version if < 2.1.0 : ask if concordance must be update
    if (version_concordance < package_version("2.1.0")){
      if (revision_codes == "HS6"){
        ask_install_concordance(version_concordance)
      }
    } # If the version is >= 2.1.0 do nothing
  }
  # If concordance is not installed : ask for the installation
  else {
    ask_install_concordance()
  }


  # Last check of the version of concorance
  # In case the user has downloaded from CRAN and want to use the HS6 revision
  version_concordance <- utils::packageVersion("concordance")
  if (version_concordance < package_version("2.1.0")){
    if (revision_codes == "HS6"){
      stop(glue::glue("Your version of the concordance package is {version_concordance}. If you want to use the HS6 revision, please restart this function and select Download from GitHub to get the 2.1.0 version. Otherwise select a revision other than HS6."))
    }
  }

  # Traiter les données de BACI ---------------------------------------------
  df_baci <-
    tradalyze::.load_data(baci) |>
    tradalyze::.filter_baci(
      years = years,
      codes = codes,
      export_countries = export_countries,
      import_countries = import_countries
    )

  # Garder les variables voulue et les variables essentielles pour les join
  if(!is.null(baci_variables)){
    df_baci <-
      df_baci |>
      dplyr::select(dplyr::all_of(baci_variables),
                    exporter, importer, k, t, v, q)
  }


  # Traiter les données de gravity ------------------------------------------
  df_gravity <- tradalyze::.load_data(gravity)


  df_gravity <-
    df_gravity |>
    # Mettre la variable year en numéric pour éviter les problèmes avec les join
    dplyr::mutate(year = as.numeric(year)) |>
    # Enelver les lignes où les pays d'origine et de destination sont les mêmes
    dplyr::filter(iso3_o != iso3_d)


  # Garder les années voulues si years != NULL
  if(!is.null(years)){
    df_gravity <-
      df_gravity |>
      dplyr::filter(year %in% years)
  }

  # Garder les variables voulue et les variables essentielles pour les join
  if(!is.null(gravity_variables)){
    df_gravity <-
      df_gravity |>
      dplyr::select(dplyr::all_of(gravity_variables), iso3_o, iso3_d, year)
  }


  # Traiter les données de PLTE ---------------------------------------------
  # Les codes de PLTE sont en HS07, il faut convertir les codes fournis
  # Si nécessaire

  # Si sélection de codes, alors faire la conversion si nécessaire et filtrer
  # Sinon, on garde tous les codes et suppose qu'il sont dans la révision HS07
  if(!is.null(codes)){
    # Si la liste des code fournie n'est pas dans la révision de 2007,
    # Faire la correspondance
    if(revision_codes != "HS3"){
      codes <-
        concordance::concord_hs(
          sourcevar = codes,
          origin = revision_codes,
          destination = "HS3",
          dest.digit = 6
        )
    }

    # Garder uniquement les élasticités des codes sélectionnés
    df_ptle <-
      tradalyze::ptle |>
      dplyr::filter(HS6 %in% codes) |>
      dplyr::select(HS6, sigma)
  }


  # Création et exportation du dataframe ------------------------------------
  # Créer le dataframe utilisable pour estimer la qualité d'un produit
  df <-
    df_baci |>
    # Mettre la variable t en numeric pour éviter les problèmes de fusion
    dplyr::mutate(t = as.numeric(t)) |>
    # Left_join avec plte pour ajouter les élasticités de chaque produit
    dplyr::left_join(
      df_ptle,
      dplyr::join_by(k == HS6)
    ) |>
    # Ajouter les variables de gravité pour chaque exporter-importer-année
    dplyr::left_join(
      df_gravity,
      dplyr::join_by(exporter == iso3_o, importer == iso3_d, t == year)
    ) |>
    dplyr::collect() |>
    # Calculer le prix et le log de la demande
    dplyr::mutate(
      p = v / q,
      demand = log(q) + (sigma * log(p))
    )

  # Supprimer les valeurs manquantes si na.rm = TRUE
  if(na.rm == TRUE){
    df <-
      df |>
      stats::na.omit()
  }

  # Export data
  tradalyze::.export_data(
    data = df,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = TRUE,
    collect = TRUE
  )
}
