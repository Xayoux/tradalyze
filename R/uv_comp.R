#' @title Calculate the Mean of a variable in a Dataframe
#'
#' @param df_baci Baci data
#' @param var_aggregate Character : vector of aggreagtions variables
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
#' @param var_aggregate Character : vector of aggreagtions variables
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


fixed_weight_computation <- function(df_baci, year_ref_fixed_weight,
                                     var_desagregation, var_pond, var_temporal){
  df_pond <-
    df_baci  |>
    dplyr::filter(
      !!sym(var_temporal) == year_ref_fixed_weight
    ) |>
    dplyr::select({{var_desagregation}}, {{var_pond}}) |>
    dplyr::select(!{{var_temporal}})

  var_join <-
    var_desagregation[var_desagregation != var_temporal]

  df_baci <-
    df_baci  |>
    dplyr::select(!{{var_pond}}) |>
    dplyr::left_join(
      df_pond,
      dplyr::join_by(var_join)
    )
    

  return(df_pond)
}




# Création d'un dataframe d'exemple
df_baci <- data.frame(
  exporter = c("ARG", "BRA", "CHN", "USA", "FRA", "ARG", "BRA", "CHN", "USA", "FRA"),
  importer = c("FRA", "USA", "ARG", "BRA", "CHN", "FRA", "USA", "ARG", "BRA", "CHN"),
  product_code = c("1001", "1002", "1003", "1004", "1005", "1001", "1002", "1003", "1004", "1005"),
  year = c(2020, 2020, 2020, 2020, 2020, 2021, 2021, 2021, 2021, 2021),
  value = c(100, 200, 300, 400, 500, 150, 250, 350, 450, 550),
  weight = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55)
)

# Affichage du dataframe
print(df_baci)












if (fixed_weight == TRUE){
    df_pond <-
      df_baci |>
      dplyr::filter(t == year_ref_fixed_weight) |>
      dplyr::select({{var_desagregate}}, {{var_pond}}) |>
      dplyr::select(-t)

    var_desagregate_join <-
      var_desagregate[var_desagregate != "t"]

    df_baci <-
      df_baci |>
      dplyr::select(-{{var_pond}}) |>
      dplyr::left_join(
        df_pond,
        by = var_desagregate_join
      )
  }


fixed_weight_computation(
  df_baci = df_baci,
  year_ref_fixed_weight = 2020,
  var_desagregation = c("exporter", "importer", "product_code", "year"),
  var_pond = "weight",
  var_temporal = "year"
)



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
                    formula = "median_pond",
                    var_pond = NULL, fixed_weight = FALSE,
                    year_ref, var_exporter, year_ref_fixed_weight = year_ref,
                     var_desagregate = c("t", "exporter", "importer", "k"),
                    var_k, exporter_ref = NULL, base_100 = TRUE,
                    compare = FALSE,
                    return_output = TRUE, return_arrow = FALSE,
                    path_output = NULL){

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



  ## Calcul des valeurs unitaires -----------------------------------------
  # Faire la moyenne des valeurs unitaires pour chaque année, exporter, produits
  if(formula == "mean"){
    df_uv <-
      df_baci |>
      dplyr::summarize(
        .by = c(t, {{var_exporter}}, {{var_k}}),
        uv = mean(uv, na.rm = TRUE)
      )
  }
  
  # Faire la médiane
  if (formula == "median"){
    df_uv <-
      df_baci |>
      dplyr::summarize(
        .by = c(t, {{var_exporter}}, {{var_k}}),
        uv = stats::median(uv, na.rm = TRUE)
      )
  }


  # Uniquement sur les calculs avec des pondérations
  # Si les poids sont fixes, calculés les poids de l'année de référence
  # Les associer aux flux
  if (fixed_weight == TRUE){
    df_pond <-
      df_baci |>
      dplyr::filter(t == year_ref_fixed_weight) |>
      dplyr::select({{var_desagregate}}, {{var_pond}}) |>
      dplyr::select(-t)

    var_desagregate_join <-
      var_desagregate[var_desagregate != "t"]

    df_baci <-
      df_baci |>
      dplyr::select(-{{var_pond}}) |>
      dplyr::left_join(
        df_pond,
        by = var_desagregate_join
      )
  }
  
  
  # Faire la médiane pondérée
  if (formula == "median_pond"){
    df_uv <-
      df_baci |>
      dplyr::collect() |>
      dplyr::summarize(
        .by = c(t, {{var_exporter}}, {{var_k}}),
        uv = matrixStats::weightedMedian(uv, w = !!dplyr::sym(var_pond), na.rm = TRUE)
      )
  }
  
  # Faire la moyenne pondérée
  if (formula == "mean_pond"){
    df_uv <-
      df_baci |>
      dplyr::collect() |>
      dplyr::summarize(
        .by = c(t, {{var_exporter}}, {{var_k}}),
        uv = stats::weighted.mean(uv, w = !!dplyr::sym(var_pond), na.rm = TRUE)
      )
  }


  ## Base 100 -------------------------------------------------------------
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
