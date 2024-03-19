#' Fonction pour créer un dataframe/fichier csv à partir des données BACI, filtré selon les critères voulus.
#'
#' @param folder_baci Un chemin d'accès vers le dossier contenant les fichiers BACI (ne pas modifier les noms des fichiers par rapport aux noms originaux).
#' @param year_start Année de début de la base de données (Si NULL alors la date de départ sera l'année la plus ancienne des fichiers BACI).
#' @param year_end Année de fin de la base de données (Si NULL alors la date de fin sera l'année la plus récente des fichiers BACI).
#' @param hs_codes Un vecteur de caractères indiquant les codes HS à garder (Si NULL alors tous les codes HS sont gardés).
#' @param exporter_codes Un vecteur de numériques indiquant les codes pays exportateurs à garder (Si NULL alors tous les pays exportateurs sont gardés).
#' @param importer_codes Un vecteur de numériques indiquant les codes pays importateurs à garder (Si NULL alors tous les pays importateurs sont gardés).
#' @param add_iso3 Un booléen indiquant si les codes iso3 doivent être ajoutés aux codes pays (TRUE par défaut).
#' @param calc_uv Un booléen indiquant si les valeurs unitaires doivent être calculées (TRUE par défaut).
#' @param path_output Un chemin d'accès vers le fichier csv de sortie (Si NULL alors aucun fichier n'est créé).
#' @param return_output Un booléen indiquant si le dataframe doit être retourné (FALSE par défaut).
#'
#' @return Un dataframe et/ou un fichier csv selon les paramètres.
#' @export
#'
#' @examples # Pas d'exemple pour cette fonction.
create_baci_db <- function(folder_baci, year_start = NULL, year_end = NULL,
                           hs_codes = NULL, exporter_codes = NULL,
                           importer_codes = NULL, add_iso3 = TRUE,
                           calc_uv = TRUE, path_output = NULL,
                           return_output = FALSE){


# Messages d'erreurs si les paramètres sont mal renseignés ----------------

  # Vérifier que le chemin vers le dossier BACI est bien renseigné
  if (missing(folder_baci)){
    stop("Le chemin vers le dossier BACI doit être renseigné")
  }
<<<<<<< HEAD
  fezfez
  # Message d'erreur si baci_folder n'est pas un dossier
  if (!dir.exists(baci_folder)){
    stop("baci_folder doit \uEAtre un dossier ou exister.")
=======

  # Vérifier si year_start est NULL ou un nombre entier
  if (!is.null(year_start) & !is.numeric(year_start)){
    stop("year_start doit être NULL ou un nombre entier")
>>>>>>> baci-db-arrow
  }

  # Vérifier si year_end est NULL ou un nombre entier
  if (!is.null(year_end) & !is.numeric(year_end)){
    stop("year_end doit être NULL ou un nombre entier")
  }

  # Vérifier si hs_codes est NULL, des chaînes de caractères
  if (!is.null(hs_codes) & !is.character(hs_codes)){
    stop("hs_codes doit être NULL, un character ou un vecteur de chaine de caractère")
  }

  # vérifier si exporter_codes est NULL ou des numériques
  if (!is.null(exporter_codes) & !is.numeric(exporter_codes)){
    stop("exporter_codes doit être NULL, un numeric ou un vecteur de numériques")
  }

  # Vérifier si importer_codes est NULL ou des numériques
  if (!is.null(importer_codes) & !is.numeric(importer_codes)){
    stop("importer_codes doit être NULL, un numeric ou un vecteur de numériques")
  }

  # Vérifier si add_iso3 est un booléen
  if (!is.logical(add_iso3)){
    stop("add_iso3 doit être un booléen")
  }

  # Vérifier si calc_uv est un booléen
  if (!is.logical(calc_uv)){
    stop("calc_uv doit être un booléen")
  }

  # Vérifier si path output est une chaîne de caractère qui se termine par .csv
  if (!is.null(path_output) & !stringr::str_detect(path_output, ".csv$")){
    stop("path_output doit être NULL ou un chemin d'accès se terminant par se terminant par .csv")
  }

  # Vérifier si return_output est un booléen
  if (!is.logical(return_output)){
    stop("return_output doit être un booléen")
  }


# Définir les fichiers BACI de travail ------------------------------------
  # Stocke les noms des fichiers BACI (toutes les années) + trie
  vector_baci_name <-
    folder_baci |>
    list.files(full.names = FALSE, pattern = "^BACI") |>
    sort()

  # Si l'année de début n'est pas spécifiée on prend l'année la plus ancienne
  if (is.null(year_start)){
    year_start <-
      # Le premier chemin correspond à l'année la plus ancienne (car trié)
      vector_baci_name[1] |>
      # On extrait la chaine de caractère 'YXXX' du nom du fichier = année
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
      # On extrait la chaine de caractère 'YXXX' = année
      stringr::str_extract("Y\\d{4}") |>
      # On retire le 'Y' pour ne garder que l'année
      stringr::str_remove("Y") |>
      # On transforme la chaîne de caractère en valeur numérique pour pouvoir créer une séquence par la suite
      as.numeric()
  }

  # Crée une expression régulière à partir des années de début et de fin pour filtrer les fichiers BACI
  # Travailler avec le minium de fichiers baci pour gagner du temps
  regex_years <-
    # Créer un vecteur contenant toutes les années de year_start à year_end
    year_start:year_end |>
    # Ajouter un 'Y' devant chaque année + transformation en chaîne de caractère
    # Match avec les noms des fichiers BACI
    purrr::map(~glue::glue("Y{.}")) |>
    # Collapser le vecteur en une seule chaîne de caractère, chaque élément séparé par '|' pour indiquer 'ou'
    stringr::str_c(collapse = "|")

  # Filtrer les chemin vers BACI pour ne garder que ceux qui correspondent aux années voulues
  vector_baci_path <-
    folder_baci |>
    list.files(pattern = regex_years, full.names = TRUE)


# Importation BACI + filtres ----------------------------------------------

  # Définir le type des données pour l'importation
  # t, i, j en integer
  # k en str pour éviter la suppression de 0 : importation automatique en int64 (pas bon)
  # q en str car pb d'espaces + "NA" : imortation automatique en str
  # v en str par mesure de précaution si ca fait la même chose que pour q
  schema_baci <-
    arrow::schema(
      arrow::Field$create("t", type = arrow::int32()),
      arrow::Field$create("i", type = arrow::int32()),
      arrow::Field$create("j", type = arrow::int32()),
      arrow::Field$create("k", type = arrow::string()),
      arrow::Field$create("v", type = arrow::string()),
      arrow::Field$create("q", type = arrow::string())
    )

  # Charger les fichiers BACI dans un seul dataframe : pas de prise de mémoire
  df_baci <-
    vector_baci_path |>
    arrow::open_csv_dataset(col_types = schema_baci)

  # Garder uniquement les codes voulus
  if (!is.null(hs_codes)){
    df_baci <-
      df_baci |>
      dplyr::filter(k %in% hs_codes)
  }

  # Garder uniquement les pays exportateurs voulus
  if (!is.null(exporter_codes)){
    df_baci <-
      df_baci |>
      dplyr::filter(i %in% exporter_codes)
  }

  # Garder uniquement les pays importateurs voulus
  if (!is.null(importer_codes)){
    df_baci <-
      df_baci |>
      dplyr::filter(j %in% importer_codes)
  }

  # Supprimer les espaces dans q (et v)
  # Mettre NA si autre chose que des chiffre et . dans q (et v)
  # Transformer q (et v) en numérique
  df_baci <-
    df_baci |>
    dplyr::mutate(
      q = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(q), "^[0-9.]+$"), NA, stringr::str_trim(q))),
      v = as.numeric(dplyr::if_else(!stringr::str_detect(stringr::str_trim(v), "^[0-9.]+$"), NA, stringr::str_trim(v)))
    )

# Mise en forme de la base ------------------------------------------------

  # Si add_iso3 = TRUE : ajouter les codes iso3 aux pays i et j pour plus de lisibilité
  if (add_iso3 == TRUE){
    df_baci <-
      df_baci |>
      # Ajouter les iso3 correspondant aux exportateurs
      dplyr::left_join(
        analyse.competitivite::country_codes_V202401,
        by = c("i" = "country_code")
      ) |>
      dplyr::rename(exporter = country_iso3) |>
      # Ajouter les iso3 correspondant aux importateurs
      dplyr::left_join(
        analyse.competitivite::country_codes_V202401,
        by = c("j" = "country_code")
      ) |>
      dplyr::rename(importer = country_iso3) |>
      # Sommer les lignes si doublons dans codes iso3 pour exportateur et importateur sur le m^me produit et même année
      # Cas si un code iso pour plusieurs codes pays
      dplyr::summarize(
        .by = c(t, exporter, importer, k),
        v = sum(v, na.rm = TRUE),
        q = sum(q, na.rm = TRUE)
      ) |>
      # Remplace les valeurs 0 par NA (si 1 seule valeur NA lors de la somme des lignes ca devient un 0)
      dplyr::mutate(
        v = if_else(v == 0, NA, v),
        q = if_else(q == 0, NA, q)
      )
  }


# Calcul valeurs unitaires ------------------------------------------------
  if (calc_uv == TRUE){
    df_baci <-
      df_baci |>
      dplyr::mutate(
        uv = v / q
      )
  }


# Enregistrer au format csv si path_output est un chemin d'accès ----------
  if (!is.null(path_output)){
    df_baci <-
      df_baci |>
      arrow::as_arrow_table() |>
      arrow::write_csv_arrow(path_output)
  }


# Return df_baci si return = TRUE -----------------------------------------
  if (return_output == TRUE){
    return(df_baci |>
             collect())
  }
}
