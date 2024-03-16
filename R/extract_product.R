#' Fonction qui permet de creer un fichier xlsx ou csv contenant tous les codes hs6 souhaites ainsi que leur description.
#'
#' @param path_product_code Chemin d'acces au fichier contenant tous les codes produits HS6 (disponible dans le dossier BACI)
#' @param codes Un vecteur contenant les codes ou numeros de chapitres que l'on souhaite extraire.
#' @param path_output Chemin d'acces au fichier de sortie. Doit etre un fichier .xlsx ou .csv.
#' @param export Un booleen indiquant si l'on souhaite exporter le fichier. Par defaut, export = TRUE.
#'
#' @return Un fichier .xlsx ou .csv contenant tous les codes HS6 souhaites ainsi que leur description.
#' @export
#'
#' @examples # Pas d'exemple disponible.
extract_product <- function(path_product_code, codes, path_output, export = TRUE){

  # Genere une erreur si path_product_code n'est pas un fichier .csv ou .xlsx
  if (!stringr::str_detect(path_output) %in% c(".xlsx$", ".csv$")) {
    stop("path_output doit etre un fichier .xlsx ou .csv")
  }

  # Genere une erreur si path_product_code ou path_output n'est pas une chaine de caractere
  if (!is.character(path_product_code)) {
    stop("path_product_code doit etre une chaine de caractere")
  }
  if (!is.character(path_output)) {
    stop("path_output doit etre une chaine de caractere")
  }

  # Importe le fichier contenant tous les codes produits HS6
  df_product_code <-
    path_product_code |>
    readr::read_csv()

  # Creer une expression reguliere pour ne garder que les codes HS6 voulus
  regex_codes <-

    # Prendre le vecteur de codes que l'on souhaite avoir
    codes |>

    # Ajouter devant chaque code pour indiquer qu'on souhaite tous les codes qui commencent par ces chiffres
    # Permet de trier par chapitres et pas uniquement par code precis
    purrr::map(~glue::glue("^{.}")) |>

    # Cree une seule chaine de caractere ou chaque code est separe par indiquant ainsi un ou exclusif
    stringr::str_c(collapse = "|")

  # Filtre le dataframe pour ne garder que les codes HS6 voulus
  df_product_code <-
    df_product_code |>
    dplyr::filter(stringr::str_detect(df_product_code, regex_codes))

  if (export == TRUE){
    # Exporte le dataframe dans le format souhaite
    if (stringr::str_detect(path_output, ".xlsx$")){
      df_product_code |>
        openxlsx::write.xlsx(path_output, row.names = FALSE)
    } else if (stringr::str_detect(path_output, ".csv$")) {
      df_product_code |>
        readr::write_csv(path_output)
    }
  }

  # Retourne le dataframe
  return(df_product_code)
}
