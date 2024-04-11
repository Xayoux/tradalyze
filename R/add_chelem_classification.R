#' @title
#' Ajoute la classification des régions du monde de CHELEM aux données de BACI
#'
#' @description
#' Ajoute la classification des régions du monde de CHELEM aux données de BACI.
#' Chaque exportateur et chaque importateurs se voient attribuer dans une région
#' géographique du monde à partir de la classification géographique utilisée par
#' la base de données [CHELEM](http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=17)
#' du [CEPII](http://www.cepii.fr/CEPII/fr/welcome.asp).
#'
#' @details
#' Les données de la base de données [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' sont exprimées au niveau des pays importateurs et exportateurs. Cependant,
#' il peut être souhaitable de regrouper les pays dans des zones géographiques
#' plus larges pour des analyses et résultats plus globaux.
#'
#' Les données de BACI sont fusionnées avec la classification de CHELEM. Les
#' régions définies sont les suivantes :
#'
#' - North America
#' - South America, central America and Caribbean
#' - European Union -> supposée à 26 de manière constante (grande-Bretgane non
#' comprise).
#' - CIS -> Commonwealth of Independent States
#' - Others in Europe
#' - North Africa
#' - Sub-Sahara Africa
#' - Near and Middle East
#' - North-East Asia
#' - South-East Asia
#' - South Asia and Pacific
#' - Australia and New Zealand
#'
#' Les pays qui ne sont pas dans la classification de CHELEM sont attribués à la
#' région "rdm" pour "reste du monde".
#'
#'
#' @param baci Peut être un  chemin d'accès vers le dossier contenant
#' les données de BACI au format parquet. Peut également être un dataframe ou
#' bien des données au format arrow (requête ou non) permettant ainsi de chaîner
#' les opérations entre elles. ce paramètre est obligatoire.
#' @param years Années à conserver. Par défaut, toutes les années sont
#' conservées.
#' @param codes Codes à conserver. Par défaut, tous les codes sont conservés.
#' @param path_output Chemin d'accès vers le dossier où sauvegarder les données
#' fusionnées en format parquet. Par défaut, les données ne sont pas s
#' auvegardées.
#' @param return_output Retourner les données fusionnées. Par défaut, les données ne
#' sont pas retournées.
#' @param return_pq Retourner les données fusionnées au format parquet. Par
#' défaut, les données sont retournées au format tibble.
#'
#' @return Les données de BACI fusionnées avec la classification de CHELEM.
#' @export
#'
#' @examples # Pas d'exemple
#' @source [Classification Chelem du CEPII](<http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=17>). Voir :
#' [de Saint Vaulry, A. (2008), “Base de données CHELEM - Commerce international du CEPII”,  Document de travail du CEPII, N°2008-09](http://www.cepii.fr/cepii/fr/publications/wp/abstract.asp?nodoc=1081)
#'
add_chelem_classification <- function(baci, years = NULL, codes = NULL,
                                      path_output = NULL, return_output = FALSE,
                                      return_pq = FALSE){

  # Messages d'erreur -------------------------------------------------------
  # Message d'erreur si years n'est pas NULL ou numérique
  if (!is.null(years) & !is.numeric(years)){
    stop("years doit \uEAtre NULL ou un vecteur num\uE9rique.")
  }

  # Message d'erreur si codes n'est pas NULL ou chaîne de caractère
  if (!is.null(codes) & !is.character(codes)){
    stop("codes doit \uEAtre NULL ou un vecteur de cha\uEEne de caract\uE8re.")
  }

  # Message d'erreur si path_output n'est pas NULL ou chaîne de caractère
  if (!is.null(path_output) & !is.character(path_output)){
    stop("path_output doit \uEAtre NULL ou une cha\uEEne de caract\uE8re.")
  }

  # Message d'erreur si return_output n'est pas un booléen
  if (!is.logical(return_output)){
    stop("return_output doit \uEAtre un bool\uE9en.")
  }

  # Message d'erreur si return_pq n'est pas un booléen
  if (!is.logical(return_pq)){
    stop("return_pq doit \uEAtre un bool\uE9en.")
  }


  # Exécution de la fonction ------------------------------------------------
  # Ouvrir les données de BACI
  if (is.character(baci) == TRUE){
    # Ouvrir les données depuis un dossier parquet
    df_baci <-
      baci |>
      arrow::open_dataset()
  }
  else if (is.data.frame(baci) == TRUE){
    # Ouvrir les données depuis un dataframe : passage en format arrow
    df_baci <-
      baci |>
      arrow::arrow_table()
  }
  else{
    # Ouvrir les données depuis format arrow : rien à faire
    df_baci <- baci
  }


  # Filtrer par les années souhaitées
  if (!is.null(years)){
    df_baci <- df_baci |>
      dplyr::filter(t %in% years)
  }

  # Filtrer par les produits souhaités
  if (!is.null(codes)){
    df_baci <- df_baci |>
      dplyr::filter(k %in% codes)
  }

  # Ajouter les classifications régions de CHELEM
  df_baci <-
    df_baci |>
    # Associer chaque exportateur à sa région
    dplyr::left_join(
      analyse.competitivite::chelem_classification,
      by = c("exporter" = "iso_country")
    ) |>
    # Si aucune région n'est associée -> mettre en reste du monde
    dplyr::mutate(
      iso_region = dplyr::if_else(is.na(iso_region), "rdm", iso_region),
      name_region = dplyr::if_else(is.na(name_region), "rdm", name_region)
    ) |>
    # Renommer les variables régions pour indiquer la dimmension importateur
    dplyr::rename(
      exporter_iso_region = iso_region,
      exporter_name_region = name_region
    ) |>
    # Associer chaque importateur à sa région
    dplyr::left_join(
      analyse.competitivite::chelem_classification,
      by = c("importer" = "iso_country")
    ) |>
    # Si aucune région n'est associée -> mettre en reste du monde
    dplyr::mutate(
      iso_region = dplyr::if_else(is.na(iso_region), "rdm", iso_region),
      name_region = dplyr::if_else(is.na(name_region), "rdm", name_region)
    ) |>
    # Renommer les variables régions pour indiquer la dimmension importateur
    dplyr::rename(
      importer_iso_region = iso_region,
      importer_name_region = name_region
    )

  # Sauvegarder les données fusionnées en format parquet
  if (!is.null(path_output)){
    df_baci |>
      dplyr::group_by(t) |>
      arrow::write_dataset(path_output)
  }

  # Retourner les données fusionnées
  if (return_output == TRUE){
    if (return_pq == TRUE){
      # Retourner en format parquet
      return(df_baci)
    }
    else {
      # Retourner en dataframe
      return(df_baci |>  dplyr::collect())
    }
  }

}

