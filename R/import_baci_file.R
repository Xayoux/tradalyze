#' Fonction qui permet d'importer un fichier BACI et de le filtrer selon les critères  choisis.
#'
#' @param path_baci_file Chemin du fichier BACI à importer.
#' @param product_code vecteur de codes HS6 utilisés pour filtrer le dataframe
#' @param exporter vecteur de numéros de pays (correspondance avec codes ISO3 trouvables dans le fichier country_codes.csv) utilisés pour filtrer le dataframe
#' @param importer vecteur de numéros de pays (correspondance avec codes ISO3 trouvables dans le fichier country_codes.csv) utilisés pour filtrer le dataframe.
#' @param path_country_codes Chemin du fichier country_codes.csv pour formater le dataframe. Si non spécifié, le dataframe ne sera pas formaté.
#'
#' @return Un dataframe contenant les données du fichier BACI importé et filtré.
#' @export
#'
#' @examples # Pas d'exemple pour cette fonction.
import_baci_file <- function(path_baci_file, product_code = NULL,
                             exporter = NULL, importer = NULL,
                             path_country_codes = NULL
                             ){

  # Message d'erreur si path_baci_file n'est pas une chaine de caractère
  if (!is.character(path_baci_file)){
    stop("path_baci_file doit \uEAtre une cha\uEEne de caract\uE8re.")
  }

  # Message d'erreur si path_baci_file n'est pas un fichier csv
  if (!stringr::str_detect(path_baci_file, ".csv$")){
    stop("path_baci_file doit \uEAtre un fichier csv.")
  }

  # Message d'erreur si product_code n'est pas un vecteur ou NULL
  if (!is.null(product_code) & !is.vector(product_code)){
    stop("product_code doit \uEAtre un vecteur ou NULL.")
  }

  # Message d'erreur si exporter n'est pas un vecteur ou NULL
  if (!is.null(exporter) & !is.vector(exporter)){
    stop("exporter doit \uEAtre un vecteur ou NULL.")
  }

  # Message d'erreur si importer n'est pas un vecteur ou NULL
  if (!is.null(importer) & !is.vector(importer)){
    stop("importer doit \uEAtre un vecteur ou NULL.")
  }

  # Message d'erreur si path_country_codes n'est pas une chaine de caractère
  if (!is.null(path_country_codes) & !is.character(path_country_codes)){
    stop("path_country_codes doit \uEAtre une cha\uEEne de caract\uE8re.")
  }

  # Message d'erreur si path_country_codes n'est pas un fichier csv
  if (!is.null(path_country_codes) & !stringr::str_detect(path_country_codes, ".csv$")){
    stop("path_country_codes doit \uEAtre un fichier csv.")
  }


  # Importer le fichier BACI csv
  df_baci <-
    path_baci_file |>
    readr::read_csv()


  # Si des codes produits sont rentrés en argument : filtrer en ne gardant que ces produits
  if (!is.null(product_code)){
    df_baci <-
      df_baci |>
      dplyr::filter(k %in% product_code)
  }


  # Si des codes exportateurs sont rentrés en argument : filtrer en ne gardant que ces exportateurs
  if (!is.null(exporter)){
    df_baci <-
      df_baci |>
      dplyr::filter(i %in% exporter)
  }


  # Si des codes importateurs sont rentrés en argument : filtrer en ne gardant que ces importateurs
  if (!is.null(importer)){
    df_baci <-
      df_baci |>
      dplyr::filter(j %in% importer)
  }


  # Si path_country_codes est spécifié : formater le dataframe avec les codes ISO3 des pays + renommer les variables
  if (!is.null(path_country_codes)){

    # Importer le fichier country_codes.csv pour la correspondance avec iso3
    df_country_codes <-
      path_country_codes |>
      readr::read_csv()

    df_baci <-
      df_baci |>

      # Associer chaque code pays à son code iso3
      dplyr::left_join(df_country_codes, by = c("i" = "country_code")) |>
      dplyr::rename(exporter = country_iso3) |>
      dplyr::left_join(df_country_codes, by = c("j" = "country_code")) |>

      # Renommer et replacer les variables
      dplyr::rename(
        importer = country_iso3,
        year = t,
        hs6 = k,
        value = v,
        quantity = q
      ) |>
      dplyr::relocate(year, exporter, importer, hs6, value, quantity, i, j)
  }

  return(df_baci)
}
