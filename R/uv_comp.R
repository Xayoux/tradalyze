#' @title Calculate the Mean of a variable in a Dataframe
#'
#' @param df_baci Baci data
#' @param var_aggregation Character : vector of aggregations variables
#' @param na.rm Exclude NA or not
#' @return Dataframe of aggregate unit values
mean_aggregation <- function(df_baci, var, var_aggregation, na.rm){
  df_baci <-
    df_baci  |>
    dplyr::summarize(
      .by = var_aggregation,
      !!var := mean(.data[[var]], na.rm = na.rm)
    )

  return(df_baci)
}


#' @title Calculate the Median of a variable in a Dataframe
#'
#' @param df_baci Baci data
#' @param var_aggregation Character : vector of aggregations variables
#' @param na.rm Exclude NA or not
#' @return Dataframe of aggregate unit values
median_aggregation <- function(df_baci, var, var_aggregation, na.rm){
  df_baci <-
    df_baci  |>
    dplyr::summarize(
      .by = var_aggregation,
      !!var := stats::median(.data[[var]], na.rm = na.rm)
    )
}

#' @title Cretate Dataframe with Fixed Weight
#'
#' @param df_baci Dataframe.
#' @param year_ref_fixed_weight Numeric : Year to keep to the fixed weight.
#' @param var_disaggregation Vector of character indicating the variables
#' corresponding to the lower level of aggregation : generally
#' `var_disaggregation = c("exporter", "importer", "k", "t")`.
#' @param var_temporal Name of the variable indicating the time.
#' @param var_weight Name of the variable to be used for the ponderation.
#' @return Dataframe with values in `var_pond` kept fixed.
fixed_weight_computation <- function(df_baci, year_ref_fixed_weight,
                                     var_disaggregation, var_weight, var_temporal){
  df_pond <-
    df_baci  |>
    dplyr::filter(
      !!dplyr::sym(var_temporal) == year_ref_fixed_weight
    ) |>
    dplyr::select({{var_disaggregation}}, {{var_weight}}) |>
    dplyr::select(!{{var_temporal}})

  var_join <-
    var_disaggregation[var_disaggregation != var_temporal]

  df_baci <-
    df_baci  |>
    dplyr::select(!{{var_weight}}) |>
    dplyr::left_join(
      df_pond,
      by = (var_join)
    )
    

  return(df_baci)
}

#' @title Calculate the Weighted Mean of a Variable in a Dataframe.
#'
#' @param df_baci Dataframe.
#' @param var Name of the variable to be aggregate by the weighted mean
#' @param var_aggregation Character : vector of aggregations variables
#' @param fixed_weight Logical indicating if the weight should be kept fixed
#' or not.
#' @param var_weight Name of the variable to be used for the ponderation.
#' @param year_ref_fixed_weight Numeric : Year to keep to the fixed weight.
#' @param var_disaggregation Vector of character indicating the variables
#' corresponding to the lower level of aggregation : generally
#' `var_disaggregation = c("exporter", "importer", "k", "t")`.
#' @param var_temporal Name of the variable indicating the time.
#' @param na.rm Logical indicating whether NA should be removed or not.
#' @return Dataframe of aggregate unit values
weighted_median_aggregation <- function(df_baci, var, var_aggregation, fixed_weight,
                                  var_weight, year_ref_fixed_weight = NULL,
                                  var_disaggregation, var_temporal, na.rm){
  # Check if matrixStats package is installed.
  rlang::check_installed("matrixStats", reason = "Necessary for the computation of weighted median.")

  # Make weight fixed or not
  if (fixed_weight == TRUE){
    df_baci <-
      df_baci |>
      fixed_weight_computation(
        year_ref_fixed_weight = year_ref_fixed_weight,
        var_disaggregation = var_disaggregation,
        var_weight = var_weight,
        var_temporal = var_temporal
      )
  }

  # Aggregate with weighted median
  df_baci <-
    df_baci |>
    dplyr::mutate(
      .by = var_aggregation,
      !!var := matrixStats::weightedMedian(.data[[var]], w = .data[[var_weight]], na.rm = na.rm)
    )

  return(df_baci)
}


#' @title Calculate the Weighted Mean of a Variable in a Dataframe.
#'
#' @param df_baci Dataframe.
#' @param var Name of the variable to be aggregate by the weighted mean
#' @param var_aggregation Character : vector of aggregations variables
#' @param fixed_weight Logical indicating if the weight should be kept fixed
#' or not.
#' @param var_weight Name of the variable to be used for the ponderation.
#' @param year_ref_fixed_weight Numeric : Year to keep to the fixed weight.
#' @param var_disaggregation Vector of character indicating the variables
#' corresponding to the lower level of aggregation : generally
#' `var_disaggregation = c("exporter", "importer", "k", "t")`.
#' @param var_temporal Name of the variable indicating the time.
#' @param na.rm Logical indicating whether NA should be removed or not.
#' @return Dataframe of aggregate unit values
weighted_mean_aggregation <- function(df_baci, var, var_aggregation, fixed_weight,
                                  var_weight, year_ref_fixed_weight = NULL,
                                  var_disaggregation, var_temporal, na.rm){

  # Make weight fixed or not
  if (fixed_weight == TRUE){
    df_baci <-
      df_baci |>
      fixed_weight_computation(
        year_ref_fixed_weight = year_ref_fixed_weight,
        var_disaggregation = var_disaggregation,
        var_weight = var_weight,
        var_temporal = var_temporal
      )
  }

  # Aggregate with weighted median
  df_baci <-
    df_baci |>
    dplyr::mutate(
      .by = var_aggregation,
      !!var := stats::weighted.mean(.data[[var]], w = .data[[var_weight]], na.rm = na.rm)
    )

  return(df_baci)
}




## # Création d'un dataframe d'exemple
## df_baci <- data.frame(
##   exporter = c("ARG", "BRA", "CHN", "USA", "FRA", "ARG", "BRA", "CHN", "USA", "FRA"),
##   importer = c("FRA", "USA", "ARG", "BRA", "CHN", "FRA", "USA", "ARG", "BRA", "CHN"),
##   product_code = c("1001", "1002", "1003", "1004", "1005", "1001", "1002", "1003", "1004", "1005"),
##   year = c(2020, 2020, 2020, 2020, 2020, 2021, 2021, 2021, 2021, 2021),
##   value = c(100, 200, 300, 400, 500, 150, 250, 350, 450, 550),
##   weight = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55)
## )

## # Affichage du dataframe
## print(df_baci)



## fixed_weight_computation(
##   df_baci = df_baci,
##   year_ref_fixed_weight = 2020,
##   var_disaggregation = c("exporter", "importer", "product_code", "year"),
##   var_pond = "weight",
##   var_temporal = "year"
## )















# Documentation -----------------------------------------------------------
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
#' @param fixed_weight Booléen indiquant si les poids utilisés pour la
#' pondération doivent être fixés à une année ou non.
#' @param year_ref_fixed_weight Année à partir dequels les poids fixes doivent
#' être calculés si nécessaire.
#' @param var_desagregate Vecteur de variables (sous forme de chaînes de
#' caractères) indiquant le niveau le plus désagrégé possible des données
#' fournies pour l'année, les exportateurs, importateurs et les produits.
#' Ces variables servent à définir les poids fixes pour chaque flux. Par
#' défaut les variables sont `c("t", "exporter", "importer", "k")`. Il n'est
#' normalement pas nécessaire de les changer si les données utilisées
#' proviennent des fonctions de ce package.
#'
#' @return Un dataframe contenant l'évolution des valeurs unitaires.
#' @export
#'
#' @examples # Pas d'exemples.
# Fonction uv_comp ---------------------------------------------------------
## Définition de la fonction -----------------------------------------------
uv_comp <- function(baci, years = NULL, codes = NULL,
                    export_countries = NULL, import_countries = NULL,
                    method = c("mean", "median", "weighted_mean", "weighted_median"),
                    var, var_aggregation,
                    var_weight = NULL, fixed_weight = FALSE,
                    year_ref, var_exporter, year_ref_fixed_weight = year_ref,
                    var_disaggregation = c("t", "exporter", "importer", "k"),
                    var_temporal,
                    var_k, exporter_ref = NULL, base_100 = TRUE,
                    compare = FALSE, na.rm = na.rm,
                    return_output = TRUE, return_arrow = FALSE,
                    path_output = NULL){

  # Check validity of method
  method <- match.arg(mathod)

  # Check validity of var_disaggregation
  var_disaggregation <- match.arg(var_disaggregation)

  # Check export parameters
  tradalyze::.export_data(
    data = NULL,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = FALSE,
    collect = NULL
  )

  # Import and filter baci data
  df_baci <-
    tradalyze::.load_data(baci)  |>
    tradalyze::.filter_baci(
      years = years,
      codes = codes,
      export_countries = export_countries,
      import_countries = import_countries
    ) |>
    dplyr::mutate(
      uv = v / q
    )


  # Compute the aggregation
  df_baci <-
    switch(
      method,
      "mean" =
        mean_aggregation(
          df_baci = df_baci,
          var = var,
          var_aggregation = var_aggregation,
          na.rm = na.rm
        ),
      "median" =
        median_aggregation(
          df_baci = df_baci,
          var = var,
          var_aggregation = var_aggregation,
          na.rm = na.rm
        ),
      "weighted_median" =
        weighted_median_aggregation(
          df_baci = df_baci,
          var = var,
          var_aggregation = var_aggregation,
          fixed_weight = fixed_weight,
          var_weight = var_weight,
          year_ref_fixed_weight = year_ref_fixed_weight,
          var_disaggregation = var_disaggregation,
          var_temporal = var_temporal,
          na.rm = na.rm
        ),
      "weighted_mean" =
        weighted_median_aggregation(
          df_baci = df_baci,
          var = var,
          var_aggregation = var_aggregation,
          fixed_weight = fixed_weight,
          var_weight = var_weight,
          year_ref_fixed_weight = year_ref_fixed_weight,
          var_disaggregation = var_disaggregation,
          var_temporal = var_temporal,
          na.rm = na.rm
        )
    )


  ## Base 100 -------------------------------------------------------------
  # Transform data into base 100 if wanted
  if (base_100 == TRUE){
    df_year_ref <-
      df_baci |>
      dplyr::filter(dplyr::sym(var_temporal) == year_ref) |>
      dplyr::select(!{{var_temporal}}) |>
      dplyr::rename(!!glue::glue("{var}_year_ref") := .data[[var]])

    var_join <-
        var_aggregation[!var_aggregation == var_temporal]

    df_baci <-
      df_baci |>
      dplyr::left_join(
        df_year_ref,
        by = var_join
      ) |>
      dplyr::mutate(
        !!glue::glue("{var}_100") := .data[[var]] / .data[[glue::glue("{var}_year_ref")]] * 100
      )


  ## Comparaison avec un pays ---------- ----------------------------------
    if (compare == TRUE){
      df_uv_exporter_ref <-
        df_uv |>
        dplyr::filter(!!dplyr::sym(var_exporter) == exporter_ref) |>
        dplyr::select(t, {{var_k}}, uv_100) |>
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
        )
    }
  }


  ## Exportation des résultats --------------------------------------------
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
  
  # Retourner le résultat
  if (return_output == TRUE){
    if (return_pq == TRUE){
      df_uv <-
        df_uv |>
        arrow::arrow_table()
    }
    return(df_uv |> dplyr::collect())
  }
}
