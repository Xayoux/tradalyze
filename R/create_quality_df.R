#' @title Création d'un dataframe servant à estimer la qualité d'un produit
#'
#' @description
#' Cette fonction permet de combiner les données des bases
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37),
#' [Gravity](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8) et
#' [Product Level Trade Elasticities](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=35)
#' du [CEPII](http://www.cepii.fr/CEPII/en/welcome.asp) afin d'obtenir un
#' dataframe prêt à l'emploi afin d'estimer (nottament) la qualité d'un produit
#' à partir d'une équation à la Khandelwal et al (2013).
#'
#' @details
#' Les données de BACI et de gravity étant volumineuses, ces dernières sont
#' traitées grâce au package arrow afin de limiter la prise de mémoire.
#' Cependant, un passage en mémoire est obligatoire à certains moments de
#' l'exécution de la fonction. Si les filtres (variables, années, codes) sont
#' trop permissifs comparés à la capacité de l'ordinateur, le processus peut
#' être fortement ralenti voir ne plus fonctionner. Dans ce cas, il est
#' recommandé de filtrer d'avantage les données et de procéder par itérations
#' successives.
#'
#'
#' @param baci Les données de BACI. Peut être un chemin vers un dossier parquet,
#' un dataframe ou un objet arrow.
#' @param gravity Les données de gravity. Peut être un chemin vers un dossier
#' parquet, un dataframe ou un objet arrow.
#' @param years Les années à garder. Par défaut, toutes les années sont gardées.
#' @param codes Les codes de produits à garder. Par défaut, tous les codes sont
#' gardés.
#' @param gravity_variables Les variables de gravité à garder. Par défaut, toutes
#' les variables sont gardées.
#' @param baci_variables Les variables de BACI à garder. Par défaut, toutes les
#' variables sont gardées. Les variables automatiquement gardées peu importe
#' la sélection sont exporter, importer, k, t, v et q afin de permettre le bon
#' déroulement de la suite.
#' @param revision_codes La révision de laquelle les codes fournis proviennent.
#' Les valeurs possibles sont "HS92", "HS96", "HS02", "HS07", "HS12", "HS17" et
#' "HS22". Par défaut, les codes sont supposés être en HS92.
#' @param print Afficher le dataframe si TRUE. Par défaut, FALSE.
#' @param return_output Retourner le dataframe si TRUE. Par défaut, TRUE.
#' @param return_parquet Retourner le dataframe en format arrow si TRUE. Par
#' défaut, FALSE.
#' @param path_output Le chemin où sauvegarder le dataframe. Par défaut, NULL.
#' @param format Le format de sauvegarde du dataframe. Les valeurs possibles
#' sont "parquet" et "csv". Par défaut, "parquet".
#' @param na.rm Supprimer les valeurs manquantes si TRUE. Par défaut, TRUE.
#'
#' @return Un dataframe prêt à l'emploi pour estimer la qualité d'un produit.
#' @export
#'
#' @examples # Pas d'exemples.
#'
#' @source [Gaulier, G. and Zignago, S. (2010) BACI: International Trade Database at the Product-Level. The 1994-2007 Version. CEPII Working Paper, N°2010-23](http://www.cepii.fr/PDF_PUB/wp/2010/wp2010-23.pdf)
#' @source [Conte, M., P. Cotterlaz and T. Mayer (2022), "The CEPII Gravity database". CEPII Working Paper N°2022-05, July 2022.](http://www.cepii.fr/DATA_DOWNLOAD/gravity/doc/Gravity_documentation.pdf)
#' @source [Fontagné L., Guimbard H. and Orefice G. (2022) Product-Level Trade Elasticities. Journal of International Economics, vol 137](https://www.sciencedirect.com/science/article/abs/pii/S0022199622000253?via%3Dihub)
create_quality_df <- function(baci, gravity, years = NULL, codes = NULL,
                              gravity_variables = NULL, baci_variables = NULL,
                              revision_codes = "HS92",
                              print = FALSE, return_output = TRUE,
                              return_parquet = FALSE, path_output = NULL,
                              format = "parquet", na.rm = TRUE){


  # Traiter les données de BACI ---------------------------------------------
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


  # Garder les années voulues si years != NULL
  if(!is.null(years)){
    df_baci <-
      df_baci |>
      dplyr::filter(t %in% years)
  }

  # Garder les codes voulus si codes != NULL
  if(!is.null(codes)){
    df_baci <-
      df_baci |>
      dplyr::filter(k %in% codes)
  }

  # Garder les variables voulue et les variables essentielles pour les join
  if(!is.null(baci_variables)){
    df_baci <-
      df_baci |>
      dplyr::select(dplyr::all_of(baci_variables),
                    exporter, importer, k, t, v, q)
  }


  # Traiter les données de gravity ------------------------------------------
  # Ouvrir les données de gravity
  if (is.character(gravity) == TRUE){
    # Ouvrir les données depuis un dossier parquet
    df_gravity <-
      gravity |>
      arrow::open_dataset()
  }
  else if (is.data.frame(gravity) == TRUE){
    # Ouvrir les données depuis un dataframe : passage en format arrow
    df_gravity <-
      gravity |>
      arrow::arrow_table()
  }
  else{
    # Ouvrir les données depuis format arrow : rien à faire
    df_gravity <- gravity
  }


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

  # créer la variable origin_concordance pour utiliser la fonction concord_hs :
  # Différence dans la façon de noter les révisions
  if (revision_codes == "HS22"){ # Uniquement si version du package de github
    origin_concordance <- "HS6"
  }

  if (revision_codes == "HS17"){
    origin_concordance <- "HS5"
  }

  else if (revision_codes == "HS12"){
    origin_concordance <- "HS4"
  }

  else if (revision_codes == "HS07"){
    origin_concordance <- "HS3"
  }

  else if (revision_codes == "HS02"){
    origin_concordance <- "HS2"
  }

  else if (revision_codes == "HS96"){
    origin_concordance <- "HS1"
  }

  else if (revision_codes == "HS92"){
    origin_concordance <- "HS0"
  }


  # Si sélection de codes, alors faire la conversion si nécessaire et filtrer
  # Sinon, on garde tous les codes et suppose qu'il sont dans la révision HS07
  if(!is.null(codes)){
    # Si la liste des code fournie n'est pas dans la révision de 2007,
    # Faire la correspondance
    if(revision_codes != "HS07"){
      codes <-
        concordance::concord_hs(
          sourcevar = codes,
          origin = origin_concordance,
          destination = "HS3",
          dest.digit = 6
        )
    }

    # Garder uniquement les élasticités des codes sélectionnés
    df_ptle <-
      analyse.competitivite::ptle |>
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


  # Afficher le df si print = TRUE
  if(print == TRUE){
    print(df)
  }

  # Sauvegarder le df si path_output != NULL
  if(!is.null(path_output)){
    # Enregistrer au format parquet avec un fichier par année
    if(format == "parquet"){
      df |>
        dplyr::group_by(t) |>
        arrow::write_dataset(
          path = path_output,
          format = "parquet"
        )
    }
    # Enregistrer au format csv
    else if(format == "csv"){
      readr::write_csv(df, path_output)
    }
  }

  # Retourner le df si return_output = TRUE
  if(return_output == TRUE){
    # Retourner le df en format arrow
    if(return_parquet == TRUE){
      df <-
        df |>
        arrow::arrow_table()

      return(df)
    }
    # retourner le df en format R
    else{
      return(df)
    }
  }
}
