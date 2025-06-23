# Utilitary functions ------------------------------------------------------
#' @title Ensure Devtools Installed
#' @description Checks if devtools is installed and installs it if not.
# This function should not be exported.
ensure_devtools_installed <- function(){
  if (!rlang::is_installed("devtools")){
    utils::install.packages("devtools")
  }
}

#' @title Install Concordance from GitHub
#' @description Installs the concordance package from GitHub.
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


# Documentation of extract_product -----------------------------------------
#' @title Create a Dataframe with the HS Codes Wanted and their Description.
#'
#' @description Take a vector of HS codes, sections or chapters and return a
#' data frame with all the corresponding 6-digit HS codes and their description.
#' These codes can be in any revision. It is also possible to make a
#' correspondence between the codes of the original revision and another
#' revision. This is useful for the
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' database because, depending on the revision of BACI chosen, the codes must
#' be in the good revision (1992 revision if you want the oldest data). The
#' concordance between revisions is done with the \link[concordance]{concord_hs}
#' function of the \link{concordance} package. The list of codes
#' is also taken from that package.
#'
#' @details To use the latest revision ("HS6" for the 2022 revision) you must
#' have a version greater or equal to 2.1.0 of the \link{concordance}
#' package. This version is not available on CRAN. To download it, please
#' uninstall the concordance package and run the following code :
#' `devtools::install_github("insongkim/concordance", dependencies=TRUE)`.
#' Make sure that you also have devtools. See the
#' [github page](https://github.com/insongkim/concordance) of the concordance
#' package. If you use the function without the correct version of the
#' concordance, you will be asked if the function can download it for you.
#' 
#' 
#' @param codes_vector A vector of characters containing, 6-digit codes,
#' sections, or chapter of the HS classification. 
#' @param path_output Path to save the output. If NULL (the default), output
#' will not be saved. Saving is possible in "csv" or "xlsx" format.
#' @param revision_origin A character indicating the HS revision of the
#' codes given in the `codes_vector parameters`. By default this is the revision
#' "HS0" (1988/1992). The value can be : "HS1" (1996), "HS2" (2022), "HS3" (2007),
#' "HS4" (2012) or "HS5" (2017). If the version of the
#' \link{concordance} package is greater than 2.1.0 you can also
#' choose "HS6" (2022). 
#' @param revision_destination Character. If "none" (the default), no
#' concordance is made between the given codes and codes from another revision.
#' If a revision number is supplied, concordance will be made.  The values for
#' the revision number are: "HS0" (1988/1992), "HS1" (1996), "HS2" (2022),
#' "HS3" (2007), "HS4" (2012) or "HS5" (2017). If the version of the
#' \link{concordance} package is greater than 2.1.0, you can also
#' select "HS6" (2022). `revision_destination` can't be the same as
#' `revision_origin`.
#' @param return_output Logical indicating whether output must be returned
#' or not. By default output is returned. 
#'
#' @return A dataframe containing the 6-digits codes and their description
#' for the wanted original revision (`revision_origin`).
#' \describe{
#'   \item{HS_revision_origin}{6-digit codes for the `revision_origin`}
#'   \item{desc_revision_origin.}{Description of the code for the `revision_origin`}
#'  }
#'
#' If a `revision_destination` is supplied. Two others columns are made :
#' \describe{
#'   \item{HS_revision_destination}{6-digit codes for the `revision_destination`}
#'   \item{desc_revision_destination.}{Description of the code for the `revision_destination`}
#'  }
#'
#' The output can also be a csv or xlsx file.
#'
#' @examples # Obtain the codes and the descriptions for the chapter 71,
#' # the section 0105 and the code 020120 for the "HS5" revision.
#' ## extract_product(
#' ##   codes_vector = c("71", "0105", "020120"),
#' ##   path_output = NULL,
#' ##   revision_origin = "HS5",
#' ##   revision_destination = "none",
#' ##   return_output = TRUE
#' ## )
#'
#' 
#' # Make a concordance between the codes of the HS5 revision and the HS0 revision
#' ## extract_product(
#' ##   codes_vector = c("71", "0105", "020120"),
#' ##   path_output = NULL,
#' ##   revision_origin = "HS5",
#' ##   revision_destination = "HS0",
#' ##   return_output = TRUE
#' ## )
#'
#' 
#' @source [Steven Liao, In Song Kim, Sayumi Miyano, Hao Zhang (2020). concordance: Product Concordance. R package version 2.0.0. https://CRAN.R-project.org/package=concordance](https://github.com/insongkim/concordance)
#'
#' @export
# Fonction ------------------------------------------------------------------
## Definition ---------------------------------------------------------------
extract_product <- function(codes_vector, path_output = NULL,
                            revision_origin = c("HS0", "HS1", "HS2", "HS3", "HS4", "HS5", "HS6"),
                            revision_destination = c("none", "HS0", "HS1", "HS2", "HS3", "HS4", "HS5", "HS6"),
                            return_output = TRUE){
  ## Error messages ---------------------------------------------------------
  # Check if revision_origin and revision_destination are allowed
  revision_origin = match.arg(revision_origin)
  revision_destination = match.arg(revision_destination)
  
  # Check the version of concordance
  # If concordance is installed check the version
  if (rlang::is_installed("concordance")){
    version_concordance <- utils::packageVersion("concordance")
    # If the version if < 2.1.0 : ask if concordance must be update
    if (version_concordance < package_version("2.1.0")){
      if (revision_origin == "HS6" || revision_destination == "HS6"){
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
    if (revision_origin == "HS6" || revision_destination == "HS6"){
      stop(glue::glue("Your version of the concordance package is {version_concordance}. If you want to use the HS6 revision, please restart this function and select Download from GitHub to get the 2.1.0 version. Otherwise select a revision other than HS6."))
    }
  }

  # Check if `codes_vector` is a character
  if (!is.character(codes_vector)){
    class_codes_vector <- class(codes_vector)
    stop(stringr::str_glue("codes_vector must be a character, not a {class_codes_vector}"))
  }

  # Message if there is other caracters than number in `codes_vector`
  codes_not_number <- codes_vector[grepl("\\D", codes_vector)]
  if (length(codes_not_number) != 0){
    message(stringr::str_glue("Be carefull, the following codes are not number and therefore will not be treated :\n\n{codes_not_number}"))
  }

  # Check if `path_output` is null or a character
  if (!is.null(path_output) & !is.character(path_output)){
    class_path_output <- class(path_output)
    stop(stringr::str_glue("path_output must be NULL or a character, not a {class_path_output}."))
  }

  # Check if the extension of `path_output` is csv or xlsx if not null
  if (is.character(path_output)){
    if (!tools::file_ext(path_output) %in% c("csv", "xlsx")){
      ext_path_output <- tools::file_ext(path_output)
      stop(stringr::str_glue("The extension of path_output (if a path is provided) must be \"csv\" or \"xlsx\", not \"{ext_path_output}\"."))
    }
  }

  # Check if `return_output` is a logical
  if (!is.logical(return_output)){
    class_return_output <- class(return_output)
    stop(stringr::str_glue("return_output must be a logicial, not a {class_return_output}."))
  }

  

  ## Extract the product list -----------------------------------------------
  # Dataframe containing all the codes for the revision origin
  df_hs_origin <-
    switch(
      revision_origin,
      "HS0" = concordance::hs0_desc,
      "HS1" = concordance::hs1_desc,
      "HS2" = concordance::hs2_desc,
      "HS3" = concordance::hs3_desc,
      "HS4" = concordance::hs4_desc,
      "HS5" = concordance::hs5_desc,
      "HS6" = concordance::hs6_desc
    )
  

  # Create the regular expression to match all products starting with the given codes.
  # You can also specify chapter codes and obtain 6-digit product codes.
  regex_codes <-
    # Take the vector of the codes/chapter we want
    codes_vector |>
    # Add '^' before each code : indicate that we want all codes beginning by this number.
    # ^62 : all the codes of the chapter 62. 
    purrr::map(~glue::glue("^{.}")) |>
    # Create only one string where each code is separated by '|' : indicating 'or'.
    stringr::str_c(collapse = "|")


  # Create the products dataframe containing all the wanted codes and their description
  df_products <-
    # Take the HS table from the origin revision (revision of the code_vector provided)
    df_hs_origin  |>
    # Filter to keep wanted codes
    dplyr::filter(
      # Keep only 6 digits codes
      nchar(code) == 6,
      # Keep only wanted codes with the regex
      stringr::str_detect(code, regex_codes)
    ) |>
    dplyr::mutate(
      # Create a variable containing the revision of the codes provided
      # Allow to add it to the name of the variables later
      revision = revision_origin,
      # Create an id for each code : allow to macth if a concordance is made.
      id = dplyr::row_number()
    )
    

  # If a concordance between two revision is wanted
  # 'Translate' the codes in the good revision, add their description
  # Add it to the products dataframe
  if (revision_destination != "none"){
    # Create the products dataframe for the new revision
    df_products_revision_destination <-
      # Take the products dataframe and keep only the codes of the first revision
      # And the ID to allow matching between first and second revision
      df_products |>
      dplyr::select(code, id) |>
      dplyr::mutate(
        # 'Translate' the codes of the origin revision to the destination revision
        code = concordance::concord_hs(
          sourcevar = code,
          origin = revision_origin,
          destination = revision_destination,
          dest.digit = 6
        ),
        # Get the description of theses codes 
        desc = concordance::get_desc(sourcevar = code, origin = revision_destination),
        # Add a variable indicating what is the revision destination
        # Allow to indicate it in the names of variables
        revision = revision_destination
      )  |>
      # Pivot wider the df to have the names indicating what is the destination revision
      # code_HS0 ; desc_HS0 for example
      tidyr::pivot_wider(
        names_from = revision,
        values_from = c(code, desc),
        names_sep = "_"
      )

    # Pivot the first dataframe and join it the second dataframe
    # To have the correspondance between origin and destination codes
    df_products <-
      # Pivot wider the df to have the names indicating what is the destination revision
      # code_HS6 ; desc_HS6 for example
      df_products |>
      tidyr::pivot_wider(
        names_from = revision,
        values_from = c(code, desc),
        names_sep = "_"
      ) |>
      # Join the df by id's
      dplyr::full_join(
        df_products_revision_destination,
        dplyr::join_by(id)
      )
  }
  # If no concordance with an other revision is wanted
  # Just pivot the products dataframe
  else {
    df_products <-
      df_products  |>  
      tidyr::pivot_wider(
        names_from = revision,
        values_from = c(code, desc),
        names_sep = "_"
      )
  }

  
  # If a `path_output` is provided  : save the dataframe
  if (!is.null(path_output)){
    path_output_ext <- tools::file_ext(path_output)

    # Save the dataframe to csv format
    if (path_output_ext == "csv"){
      rlang::check_installed("readr", reason = "\n\nNecessary to write the output in csv format.")
      readr::write_csv(
        df_products,
        path_output
      )
    }
    # Save the dataframe to xlsx format
    else if (path_output_ext == "xlsx"){
      rlang::check_installed("writexl", reason = "\n\nNecessary to write the output in xlsx format.")
      writexl::write_xlsx(
        df_products,
        path_output
      )
    }
  }

  # Return the dataframe if wanted
  if (return_output == TRUE){
    return(df_products)
  }
}

utils::globalVariables(c("id", "revision", "desc"))
