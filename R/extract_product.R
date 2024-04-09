#' Créer un fichier xlsx ou csv contenant tous les codes hs6 souhaités ainsi que leur descriptionet leur équivalent dans une autre nomenclature.
#'
#' @param codes_vector Un vecteur contenant les codes ou numéros de chapitres que l'on souhaite extraire.
#' @param path_output Chemin d'accès au fichier de sortie. Doit être un fichier .xlsx ou .csv.
#' @param revision_origin Un caractère indiquant la révision des codes HS d'origine. Par défaut, revision_origin = "HS22".
#' Les valeurs possibles sont "HS22", "HS92", "HS96", "HS02", "HS07", "HS12" et "HS17".
#' @param revision_destination Un caractère indiquant la révision des codes HS de destination.
#' Par defaut, revision_destination = NULL. Les valeurs possibles sont "HS92", "HS96", "HS02", "HS07", "HS12" et "HS17".
#' Si NULL alors, il n'y a pas de conversion des codes dans une autre révision.
#' @param export Un booléen indiquant si l'on souhaite exporter le fichier. Par défaut, export = TRUE.
#' @param return_df Un booléen indiquant si l'on souhaite retourner le dataframe. Par défaut, return_df = TRUE.
#' @param correspondance Un booléen qui indique si l'on souhaite convertir les codes HS6 données
#' en des codes HS6 d'une autre révision. Par defaut, correspondance = FALSE. Si TRUE alors, revision_destination ne
#' doit pas être NULL (et inversement).
#'
#' @return Un fichier .xlsx ou .csv contenant tous les codes HS6 souhaités ainsi que leur description.
#' \describe{
#'   \item{HS...}{codes HS6 de la nomenclature ...}
#'   \item{description_HS...}{description des codes HS6 de la nomenclature ...}
#'  }
#' @export
#'
#' @examples # Pas d'exemple disponible.
#' @source [Steven Liao, In Song Kim, Sayumi Miyano, Hao Zhang (2020). concordance: Product Concordance. R package version 2.0.0. https://CRAN.R-project.org/package=concordance](https://github.com/insongkim/concordance)
extract_product <- function(codes_vector, path_output, revision_origin = "HS22",
                            revision_destination = NULL,
                            export = TRUE, return_df = TRUE, correspondance = FALSE){


# Générer des erreurs si les paramètres sont mal remplis ------------------

    # Genere une erreur si path_output n'est pas un fichier .csv ou .xlsx
  if (!stringr::str_detect(path_output, ".xlsx$|.csv$")) {
    stop("path_output doit \uEAtre un fichier .xlsx ou .csv")
  }

  # Genere une erreur si path_output n'est pas une chaine de caractere
  if (!is.character(path_output)) {
    stop("path_output doit \uEAtre une cha\uEEne de caract\uE8re")
  }

  # Génère une erreur si revision_origin n'est pas un caractère
  if (!is.character(revision_origin)){
    stop("revision_origine doit \uEAtre un caract\uE8re.")
  }

  # génère une erreur si revision_origin n'est pas un des caractères suivants
  if (!revision_origin %in% c("HS22", "HS92", "HS96", "HS02", "HS07", "HS12", "HS17")){
    stop("hs_revision doit \uEAtre un des caract\uE8res suivants : 'HS22', 'HS92', 'HS96', 'HS02', 'HS07', 'HS12', 'HS17'.")
  }

  # génère une erreur si revision_destination n'est pas un caractère ou NULL
  if (!is.character(revision_destination) & !is.null(revision_destination)){
    stop("revision_destination doit \uEAtre un caract\uE8re ou NULL.")
  }

  # Génère une erreur si revision_destination n'est pas un des caractères suivants s'il s'agit d'un caractère
    if (!is.null(revision_destination)){
    if (!revision_destination %in% c("HS92", "HS96", "HS02", "HS07", "HS12", "HS17") & is.character(revision_destination)){
      stop("hs_revision doit \uEAtre un des caract\uE8res suivants : 'HS92', 'HS96', 'HS02', 'HS07', 'HS12', 'HS17'. Ou bien doit \uEAtre NULL")
    }
  }

  # Génère une erreur si correspondance = TRUE et revision_destination n'est pas un des caractères suivants
  if (correspondance == TRUE & is.null(revision_destination)){
    stop("Si correspondance = TRUE, revision_destination ne peut pas \uEAtre NULL.")
  }

  # Génère un avertissement si correspondance = FALSE et revision_destination n'est pas NULL
  if (correspondance == FALSE & !is.null(revision_destination)){
    warning("Si correspondance = FALSE, revision_destination doit \uEAtre NULL.")
  }



# Sélectionner la db de codes HS à utiliser pour la sélection de codes--------

  # Si revision_origin = "HS22", utiliser la base de donnée product_codes_HS22_V202401.rda du package
  if (revision_origin == "HS22"){
    df_product_code <- analyse.competitivite::product_codes_HS22_V202401
  }

  # Sinon si revision_origin = "HS92", utiliser la base de donnée product_codes_HS92_V202401.rda du package
  else if (revision_origin == "HS92"){
    df_product_code <- analyse.competitivite::product_codes_HS92_V202401
  }

  # Sinon si revision_origin = "HS96", utiliser la base de donnée product_codes_HS96_V202401.rda du package
  else if (revision_origin == "HS96"){
    df_product_code <- analyse.competitivite::product_codes_HS96_V202401
  }

  # Sinon si revision_origin = "HS02", utiliser la base de donnée product_codes_HS02_V202401.rda du package
  else if (revision_origin == "HS02"){
    df_product_code <- analyse.competitivite::product_codes_HS02_V202401
  }

  # Sinon si revision_origin = "HS07", utiliser la base de donnée product_codes_HS07_V202401.rda du package
  else if (revision_origin == "HS07"){
    df_product_code <- analyse.competitivite::product_codes_HS07_V202401
  }

  # Sinon si revision_origin = "HS12", utiliser la base de donnée product_codes_HS12_V202401.rda du package
  else if (revision_origin == "HS12"){
    df_product_code <- analyse.competitivite::product_codes_HS12_V202401
  }

  # Sinon si revision_origin = "HS17", utiliser la base de donnée product_codes_HS17_V202401.rda du package
  else if (revision_origin == "HS17"){
    df_product_code <- analyse.competitivite::product_codes_HS17_V202401
  }



# Création du fichier contenant les codes ---------------------------------

  # Creer une expression reguliere pour ne garder que les codes HS6 voulus
  regex_codes <-
    # Prendre le vecteur de codes/chapitres que l'on souhaite avoir
    codes_vector |>
    # Ajouter '^' devant chaque code pour indiquer qu'on souhaite tous les codes qui commencent par ces chiffres
    # Permet de trier par chapitres et pas uniquement par code precis
    purrr::map(~glue::glue("^{.}")) |>
    # Créer une seule chaine de caractère où chaque code est séparé par '|' indiquant ainsi un 'ou' exclusif
    stringr::str_c(collapse = "|")


  # Créer le dataframe qui va contenir les codes voulus et leur description
  df_product_code <-
    df_product_code |>
    # Filtre le dataframe pour ne garder que les codes HS6 voulus
    dplyr::filter(stringr::str_detect(code, regex_codes))


  # Si correspondance = TRUE : faire la correspondance entre les deux révisions voulues
  if(correspondance == TRUE){
    # sélectionner la db de codes hs à utiliser pour les description des codes de destination
    if (revision_destination == "HS22"){
      df_product_code_destination <- analyse.competitivite::product_codes_HS22_V202401
    }

    else if (revision_destination == "HS92"){
      df_product_code_destination <- analyse.competitivite::product_codes_HS92_V202401
    }

    else if (revision_destination == "HS96"){
      df_product_code_destination <- analyse.competitivite::product_codes_HS96_V202401
    }

    else if (revision_destination == "HS02"){
      df_product_code_destination <- analyse.competitivite::product_codes_HS02_V202401
    }

    else if (revision_destination == "HS07"){
      df_product_code_destination <- analyse.competitivite::product_codes_HS07_V202401
    }

    else if (revision_destination == "HS12"){
      df_product_code_destination <- analyse.competitivite::product_codes_HS12_V202401
    }

    else if (revision_destination == "HS17"){
      df_product_code_destination <- analyse.competitivite::product_codes_HS17_V202401
    }

    # créer la variable destination_concordance pour utiliser la fonction concord_hs : différence dans la façon de noter les révisions
    if (revision_destination == "HS22"){ # Uniquement si version du package de github
      destination_concordance <- "HS6"
    }

    if (revision_destination == "HS17"){
      destination_concordance <- "HS5"
    }

    else if (revision_destination == "HS12"){
      destination_concordance <- "HS4"
    }

    else if (revision_destination == "HS07"){
      destination_concordance <- "HS3"
    }

    else if (revision_destination == "HS02"){
      destination_concordance <- "HS2"
    }

    else if (revision_destination == "HS96"){
      destination_concordance <- "HS1"
    }

    else if (revision_destination == "HS92"){
      destination_concordance <- "HS0"
    }

    # Créer la variable origin_concordance pour utiliser la fonction concord_hs : différence dans la façon de noter les révisions
    if (revision_origin == "HS22"){ # uniquement si version du package de github
      origin_concordance <- "HS6"
    }

    if (revision_origin == "HS17"){
      origin_concordance <- "HS5"
    }

    else if (revision_origin == "HS12"){
      origin_concordance <- "HS4"
    }

    else if (revision_origin == "HS07"){
      origin_concordance <- "HS3"
    }

    else if (revision_origin == "HS02"){
      origin_concordance <- "HS2"
    }

    else if (revision_origin == "HS96"){
      origin_concordance <- "HS1"
    }

    else if (revision_origin == "HS92"){
      origin_concordance <- "HS0"
    }

    # Créer le dataframe qui va contenir les codes voulus, leur correspondance avec la révision voulue et leur description
    df_product_code <-
      df_product_code |>
      # Utilise la fonction concord_hs pour trouver la correspondance entre les codes HS6 voulus et la révision voulue
      dplyr::mutate(
        code_destination = concordance::concord_hs(
          sourcevar = code,
          origin = origin_concordance,
          destination = destination_concordance,
          dest.digit = 6
        )
      ) |>
      # Ajoute la description des codes de destination
      dplyr::left_join(df_product_code_destination, by = c("code_destination" = "code"))



    # Renommer la colonne code par la valeur de la variable revision_origin
    colnames(df_product_code)[colnames(df_product_code) == "code"] <- revision_origin

    # Renommer la colonne code_destination par la valeur de la variable revision_destination
    colnames(df_product_code)[colnames(df_product_code) == "code_destination"] <- revision_destination

    # Renommer la colonne description par la valeur de la variable revision_origin
    colnames(df_product_code)[colnames(df_product_code) == "description.x"] <- glue::glue("description_{revision_origin}")

    # Renommer la colonne description par la valeur de la variable revision_destination
    colnames(df_product_code)[colnames(df_product_code) == "description.y"] <- glue::glue("description_{revision_destination}")
  }

  # Si correspondance = FALSE : juste renommer les deux variables pour inclure la révision
  else{
    # Renommer la colonne code par la valeur de la variable revision_origin
    colnames(df_product_code)[colnames(df_product_code) == "code"] <- revision_origin

    # Renommer la colonne description par la valeur de la variable revision_origin
    colnames(df_product_code)[colnames(df_product_code) == "description"] <- glue::glue("description_{revision_origin}")
  }


  # Exporte le dataframe si export = TRUE
  if (export == TRUE){
    # Exporte le dataframe dans le format souhaite
    if (stringr::str_detect(path_output, ".xlsx$")){
      df_product_code |>
        openxlsx::write.xlsx(path_output, rowNames = FALSE)
    }

    else if (stringr::str_detect(path_output, ".csv$")) {
      df_product_code |>
        readr::write_csv(path_output)
    }
  }


  # Retourne le dataframe si return_df = TRUE
  if (return_df == TRUE){
    return(df_product_code)
  }
}
