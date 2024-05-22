#' @title
#' Comparaison des valeurs unitaires
#'
#' @description
#' Compare la moyenne des valeurs unitaires par produit ou groupe de produits
#' défini. La comparaison peut se faire en valeur, en base 100, ou en ratio par
#' par rapport à un pays de référence.
#'
#' @details
#' Un déterminant de l'évolution des exportations a trait à la compétitivité
#' prix, soit les prix fixés par les exportateurs par rapport à leurs
#' compétiteurs sur les marchés étrangers. Cette dimension combine les
#' évolutions de coût de production unitaire des entreprises, de leurs marges
#' et des évolutions du taux de change. Les valeurs unitaires du commerce
#' offrent une mesure de ces prix moyens à l'exportation.
#'
#' Cette fonction prend comme input un dataframe ayant obligatoirement une
#' variable `t` contenant les années, ainsi qu'une variable `uv` contenant les
#' valeurs unitaires préalablement calculées. Ces variables sont directement
#' inclues dans les dataframes obtenus à partir des fonctions de ce package.
#'
#' @param baci Chemin d'accès, dataframe ou format parquet des données de baci
#' à utiliser.
#' @param years Années à garder dans les données.
#' @param codes Codes à garder dans les données.
#' @param year_ref Année de référence pour le calcul de la base 100.
#' @param var_exporter Variable contenant les exportateurs.
#' @param var_k Variable à utiliser pour le groupement des produits.
#' @param exporter_ref Exportateur de référence pour le calcul du ratio de la
#' base 100.
#' @param base_100 Booléen indiquant si les valeurs unitaires doit être calculée
#' en base 100 par rapport à l'année de référence.
#' @param compare Booléen indiquant si le ratio des base 100 des valeurs
#' unitaires doit être calculé par rapport à un pays de référence.
#' @param return_output Booléen indiquant si le résultat doit être retourné.
#' @param return_pq Booléen indiquant si le résultat doit être retourné en format
#' parquet.
#' @param path_output Chemin d'accès pour enregistrer le résultat. Peut être
#' en format csv ou parquet.
#' @param formula Formule à utiliser pour calculer les valeurs unitaires. Peut
#' être "mean" pour la moyenne, "median" pour la médiane, "mean_pond" pour la
#' moyenne pondérée, ou "median_pond" pour la médiane pondérée.
#' @param var_pond Variable à utiliser pour la pondération des valeurs unitaire
#' lorsque formula demande une pondération.
#'
#' @return Un dataframe contenant l'évolution des valeurs unitaires.
#' @export
#'
#' @examples # Pas d'exemples.
uv_comp <- function(baci, years = NULL, codes = NULL, formula = "median_pond",
                    var_pond = NULL, year_ref, var_exporter,
                            var_k, exporter_ref = NULL, base_100 = TRUE,
                            compare = FALSE,
                            return_output = TRUE, return_pq = FALSE,
                            path_output = NULL){

  # Ouvrir les données de BACI
  if (is.character(baci) == TRUE){
    # Ouvrir les données depuis un dossier parquet (si chemin c'est parquet)
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

  # Faire la moyenne des valeurs unitaires pour chaque année, exporter, produits
  if(formula == "mean"){
    df_uv <-
      df_baci |>
      dplyr::summarize(
        .by = c(t, {{var_exporter}}, {{var_k}}),
        uv = mean(uv, na.rm = TRUE)
      )
  }
  else if (formula == "median"){
    df_uv <-
      df_baci |>
      dplyr::summarize(
        .by = c(t, {{var_exporter}}, {{var_k}}),
        uv = stats::median(uv, na.rm = TRUE)
      )
  }
  else if (formula == "median_pond"){
    df_uv <-
      df_baci |>
      dplyr::collect() |>
      dplyr::summarize(
        .by = c(t, {{var_exporter}}, {{var_k}}),
        uv = matrixStats::weightedMedian(uv, w = !!dplyr::sym(var_pond), na.rm = TRUE)
      )
  }
  else if (formula == "mean_pond"){
    df_uv <-
      df_baci |>
      dplyr::collect() |>
      dplyr::summarize(
        .by = c(t, {{var_exporter}}, {{var_k}}),
        uv = stats::weighted.mean(uv, w = !!dplyr::sym(var_pond), na.rm = TRUE)
      )
  }



  # Mettre en base 100 si voulu
  if (base_100 == TRUE){
    df_uv_year_ref <-
      df_uv |>
      dplyr::filter(t == year_ref) |>
      dplyr::select(-t) |>
      dplyr::rename(uv_year_ref = uv)

    df_uv <-
      df_uv |>
      dplyr::left_join(
        df_uv_year_ref,
        dplyr::join_by({{var_exporter}}, {{var_k}})
      ) |>
      dplyr::mutate(
        uv_100 = uv / uv_year_ref * 100
      ) |>
      dplyr::select(-c(uv_year_ref, uv))

    if (compare == TRUE){
      df_uv_exporter_ref <-
        df_uv |>
        dplyr::filter(!!dplyr::sym(var_exporter) == exporter_ref) |>
        dplyr::select(-{{var_exporter}}) |>
        dplyr::rename(uv_100_exporter_ref = uv_100)

      df_uv <-
        df_uv |>
        dplyr::filter(!!dplyr::sym(var_exporter) != exporter_ref) |>
        dplyr::left_join(
          df_uv_exporter_ref,
          dplyr::join_by(t, {{var_k}})
        ) |>
        dplyr::mutate(
          uv_100_diff = uv_100 / uv_100_exporter_ref
        ) |>
        dplyr::select(-c(uv_100_exporter_ref, uv_100))
    }
  }

  # Retourner le résultat
  if (return_output == TRUE){
    if (return_pq == TRUE){
      df_uv <-
        df_uv |>
        arrow::arrow_table()
    }
    return(df_uv |> dplyr::collect())
  }

  # Enregistrer le résultat
  if (!is.null(path_output)){
    if (tools::file_ext(path_output) == "csv"){
      df_uv |>
        readr::write_csv(path_output)
    }
    else if (tools::file_ext(path_output) == "pq"){
      df_uv |>
        arrow::write_parquet(path_output)
    }
  }
}
