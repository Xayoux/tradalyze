#' Fonction qui permet d'importer un fichier BACI et de le filtrer selon les critères  choisis.
#'
#' @param path_baci_file Chemin du fichier BACI à importer.
#' @param product_code vecteur de codes HS6 utilisés pour filtrer le dataframe
#' @param exporter vecteur de numéros de pays (correspondance avec codes ISO3 trouvables dans le fichier country_codes.csv) utilisés pour filtrer le dataframe
#' @param importer vecteur de numéros de pays (correspondance avec codes ISO3 trouvables dans le fichier country_codes.csv) utilisés pour filtrer le dataframe.
#'
#' @return Un dataframe contenant les données du fichier BACI importé et filtré.
#' @export
#'
#' @examples # Pas d'exemple pour cette fonction.
import_baci_file <- function(path_baci_file, product_code = NULL, exporter = NULL, importer = NULL){
  df_baci <-
    path_baci_file |>
    readr::read_csv()

  if (!is.null(exporter)){
    df_baci <-
      df_baci |>
      dplyr::filter(i %in% exporter)
  }

  if (!is.null(importer)){
    df_baci <-
      df_baci |>
      dplyr::filter(j %in% importer)
  }

  if (!is.null(product_code)){
    df_baci <-
      df_baci |>
      dplyr::filter(k %in% product_code)
  }

  return(df_baci)
}
