#' Fonction qui permet de créer la databse BACI à partir des fichiers BACI et des critères choisis.
#'
#' @param baci_folder Chemin du dossier contenant les fichiers BACI (Ne modifier aucun nom par rapport à ce qui est téléchargé sur le site de la CEPII : http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37).
#' @param year_start Année de début de la base de données. Si non spécifiée, l'année la plus ancienne sera utilisée.
#' @param year_end Année de fin de la base de données. Si non spécifiée, l'année la plus récente sera utilisée.
#' @param product_code vecteur de codes HS6 utilisés pour filtrer le dataframe. Peut également être le dataframe créé par la fonction 'extract_product'.
#' @param exporter vecteur de numéros de pays (correspondance avec codes ISO3 trouvables dans le fichier country_codes.csv) utilisés pour filtrer le dataframe.
#' @param importer vecteur de numéros de pays (correspondance avec codes ISO3 trouvables dans le fichier country_codes.csv) utilisés pour filtrer le dataframe.
#' @param path_output Chemin où sauvegarder le dataframe. Si non spécifié, le dataframe ne sera pas sauvegardé.
#'
#' @return Un dataframe contenant les données de la base de données BACI importée et filtrée.
#' @export
#'
#' @examples # Pas d'exemple pour cette fonction.
create_baci_db <- function(baci_folder, year_start = NULL, year_end = NULL,
                           product_code, exporter = NULL, importer = NULL, path_output = NULL){

  # Stocke les noms des fichiers BACI (toutes les années)
  vector_baci_name <-
    baci_folder |>
    list.files(full.names = FALSE, pattern = "^BACI") |>
    sort()


  # Si l'année de début n'est pas spécifiée on prend l'année la plus ancienne
  if (is.null(year_start)){
    year_start <-

      # Le premier chemin correspond à l'année la plus ancienne (car trié)
      vector_baci_name[1] |>

      # On extrait la chaine de caractère 'YXXX' du nom du fichier correspondant à l'année la plus ancienne
      stringr::str_extract("Y\\d{4}") |>

      # On retire le 'Y' pour ne garder que l'année
      stringr::str_remove("Y") |>

      # On transforme la chaîne de caractère en valeur numérique pour pouvoir créer une séquence par la suite
      as.numeric()
  }


  # Si l'année de fin n'est pas spécifiée on prend l'année la plus récente
  if (is.null(year_end)){
    year_end <-

      # Le dernier chemin correspond à l'année la plus récente (car trié)
      vector_baci_name[length(vector_baci_name)] |>

      # On extrait la chaine de caractère 'YXXX' du nom du fichier correspondant à l'année la plus récente
      stringr::str_extract("Y\\d{4}") |>

      # On retire le 'Y' pour ne garder que l'année
      stringr::str_remove("Y") |>

      # On transforme la chaîne de caractère en valeur numérique pour pouvoir créer une séquence par la suite
      as.numeric()
  }


  # Crée une expression regulière à partir des années de début et de fin pour filtrer les fichiers BACI
  regex_years <-

    # Créer un vecteur contenant toutes les années de year_start à year_end
    year_start:year_end |>

    # Ajouter un 'Y' devant chaque année + transformation en chaîne de caractère
    purrr::map(~glue::glue("Y{.}")) |>

    # Collapser le vecteur en une seule chaîne de caractère, chaque élément séparé par '|' pour indiquer 'ou'
    stringr::str_c(collapse = "|")


  # Filtrer les fichiers BACI pour ne garder que ceux qui correspondent aux années voulues
  vector_baci_path <-
    baci_folder |>
    list.files(pattern = regex_years, full.names = TRUE)

  # Si product_code est un dataframe, on ne garde que les codes HS6 (part du principe que le dataframe contient une colonne 'code' car créé par la fonction 'extract_product")
  if (is.data.frame(product_code)){
    product_code <- unique(product_code$code)
  }


  # Importer les fichiers BACI et les filtrer selon les critères choisis
  df_baci <-
    vector_baci_path |>

    # Appliquer la fonction import_baci_file à chaque élément du vecteur vector_baci_path
    purrr::pmap(vector_baci_path, product_code, exporter, importer, import_baci_file) |>

    # Collapser les dataframes en un seul
    purrr::list_rbind()


  # Sauvegarder le dataframe si path_output est spécifié
  if (!is.null(path_output)){
    df_baci |>
      readr::write_csv(path_output)
  }


  # Retourner le dataframe
  return(df_baci)
}





