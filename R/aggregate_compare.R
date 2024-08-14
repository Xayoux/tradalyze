#' @title Calculate the Mean of a variable in a Dataframe
#'
#' @param df_baci Baci data
#' @param var Variable to be aggregated
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
#' @param var Variable to be aggregated
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


  # Check if var_weight is character and length 1
  tradalyze::.check_character(var_weight, "var_weight")
  tradalyze::.check_length_1(var_weight, "var_weight")

  # Check if year_ref_fixed_weight is numeric and length 1
  tradalyze::.check_numeric(year_ref_fixed_weight, "year_ref_fixed_weight")
  tradalyze::.check_length_1(year_ref_fixed_weight, "year_ref_fixed_weight")

  # Check if var_disaggregation is a character
  tradalyze::.check_character(var_disaggregation, "var_disaggregation")
  
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
    dplyr::summarize(
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
    dplyr::summarize(
      .by = var_aggregation,
      !!var := stats::weighted.mean(.data[[var]], w = .data[[var_weight]], na.rm = na.rm)
    )

  return(df_baci)
}




# Documentation -----------------------------------------------------------
#' @title
#' Aggregate a variable and Compare it in Base 100.
#'
#' @description
#' Aggregate a variable in a dataframe using one of the following method :
#' - mean
#'
#' - median
#'
#' - weighted mean
#'
#' - weighted median
#'
#' This variable can be transformed in base 100 to allow time comparison.
#' This base 100 ban also be compared to a reference (by a ratio). The level of
#' aggregation is freely choosen. Remember that this function (and so the
#' name and description of parameters) had been think with the idea of beeing
#' used with a database like BACI and so with variables like "exporter",
#' "k" (product) or "t" (time).
#'
#' @details
#' This feature uses [arrow](https://arrow.apache.org/docs/r/) functionalities.
#' However, the computation of the various metrics must be in memory
#' (only this part is in memory). This can take some time depending on your
#' configuration and the size of your data. If the size of the data is too
#' large for your computer, it may crash. It is advisable to reduce the size
#' of your database and run this function several times.
#'
#' @param method Character indicating the method to be used to aggregate
#' the choosen variable. It can be "mean" (the default) to compute the mean ;
#' "median" to compute the median ; "weighted_mean" to commpute a weighted mean
#' (with fixed weight or not) ; "weighted_median" to compute a weighted median
#' (with fixed weight or not).
#' @param var Character indicating the name of the variable to be aggregated.
#' @param var_aggregation Character vector containing the name of variables used
#' as a group to aggregate the data. Example : if `var_aggregation = c("t", "k",
#' "exporter")`, the variable will be aggregated at the time, product, exporter
#' level.
#' @param var_temporal Character indicating the name of the temporal
#' variable in the data. Generally it will be "t".
#' @param var_weight Character indicating the name of the variable used
#' as weight. Used only if `method = "weighted_mean"` or
#' `method = "weighted_median"`. If not used it can be set to NULL.
#' @param fixed_weight Logical indicating whether the weight should be kept fixed (TRUE) or
#' not (FALSE : the default). Used only if `method = "weighted_mean"` or
#' `method = "weighted_median"`. If TRUE, the weight will be the same each year
#' and will correspond to the weight of the year indicating in `year_ref_fixed_weight`.
#' @param year_ref_fixed_weight Numeric indicating the year of reference
#' for fixed weight. Used only if `fixed_weight = TRUE` and `method = "weighted_mean"`
#' or `method = "weighted_median"`. If not used it can be set to NULL.
#' @param var_disaggregation Character vector indicating the name variables
#' corresponding of the lower level of aggregation possible in the data.
#' Generally, if BACI is taken, it will be
#' `var_disaggregation = c("t", "exporter", "importer", "k")`. Used only
#' if `fixed_weight = TRUE`.
#' @param base_100 Logical indicating whether the aggregated variable must be
#' transformed in base 100 (TRUE) or not (FALSE : the default). If TRUE two new
#' variables will be added :
#' \describe{
#'   \item{'var'_year_ref}{Numeric : Value of the aggregate variable in the
#' reference year defined by `year_ref_base_100` parameter. The name of the variable
#' depends on the name the aggregated variable. If the aggregated variable
#' is "uv", the name of this variable will be "uv_year_ref".}
#'   \item{'var_100'}{Numeric : Value of the aggregated variable transformed
#' in base 100. The name of the variable depends on the name the aggregated
#' variable. If the aggregated variable is "uv", the name of this variable will
#' be "uv_year_ref".}
#' }
#' @param year_ref_base_100 Numeric indicating the reference year at which the
#' base 100 begin. Use only if `base_100 = TRUE`. If not used, it can be set on
#' NULL.
#' @param compare Logical indicating if the base 100 should be compared (TRUE)
#' by a ratio, to the base 100 of a reference exporter, defined by the parameter
#' `exporter_ref`, or not (FALSE the default). If TRUE, two nex variables are
#' added
#' \describe{
#'   \item{'var'_exporter_ref}{Numeric : Value of the aggregate variable for the
#' reference exporter defined by `exporter_ref` parameter. The name of the variable
#' depends on the name the aggregated variable. If the aggregated variable
#' is "uv", the name of this variable will be "uv_exporter_ref".}
#'   \item{'var_100_diff'}{Numeric : Value of the ratio between the base 100 and
#' the base 100 of the reference exporter. A value greater than 1 indicate that
#' the the variable for the exporter has increased more than the variable for
#' the reference exporter. The name of the variable depends on the name of the aggregated
#' variable. If the aggregated variable is "uv", the name of this variable will
#' be "uv_100_diff".}
#' }
#' @param var_exporter Character indicating the name of the variable containing
#' the exporters (or the the variable containing entities you want to compare).
#' Used only if `compare = TRUE`. If used, the variable used must alos
#' be present `var_aggregate`. if not used it can be set on NULL.
#' @param exporter_ref Character indicating the reference exporter (or entity)
#' present in the variable given in `var_exporter`. Used only if
#' `compare = TRUE`. if not used it can be set on NULL.
#' @inheritParams .filter_baci
#' @inheritParams .export_data
#' @inheritParams add_chelem_classification
#' @inheritParams clean_uv_outliers
#'
#' @return BAci data with the following variables : variables contained in
#' `var_aggregation`, `var`. Depending on the value taken by `base_100` and
#' `compare` 2 or 4 more variables can be present. See the explications of these
#' parameters.
#'
#' @examples
#' ## Aggregate a variable name "uv" with "mean" at the exporter-year-product
#' ## level. BACI data are used.
#' ## aggregate_compare(
#' ##   baci = "baci-foler-parquet",
#' ##   method = "mean",
#' ##   var = "uv",
#' ##   var_aggregation = c("exporter", "t", "k"),
#' ##   var_temporal = "t",
#' ##   base_100 = FALSE,
#' ##   na.rm = TRUE
#' ## )
#'
#' ## Aggregate a variable name "uv" with "weighted_mean" at the
#' ## region-year-category level with free weight with weight contained in "q".
#' ## BACI data are used. Base 100 is computed and begin in 2015. 
#' ## aggregate_compare(
#' ##   baci = "baci-foler-parquet",
#' ##   method = "weighted_mean",
#' ##   var = "uv",
#' ##   var_aggregation = c("region", "t", "category"),
#' ##   var_temporal = "t",
#' ##   var_weight = "q",
#' ##   fixed_weight = FALSE,
#' ##   base_100 = TRUE,
#' ##   year_ref_base_100 = 2015,
#' ##   compare = FALSE,
#' ##   na.rm = TRUE
#' ## )
#'
#' ## Aggregate a variable name "uv" with "weighted_median" at the
#' ## exporter-year-product level with fixed weight with weight in 2010
#' ## contained in "q". BACI data are used. 
#' ## Base 100 is computed and begin in 2015 and base 100 is compared to France
#' ## aggregate_compare(
#' ##   baci = "baci-foler-parquet",
#' ##   method = "weighted_mean",
#' ##   var = "uv",
#' ##   var_aggregation = c("exporter", "t", "k"),
#' ##   var_temporal = "t",
#' ##   var_weight = "q",
#' ##   fixed_weight = TRUE,
#' ##   year_ref_fixed_weight = 2010,
#' ##   var_disaggregation = c("t", "exporter", "importer", "k"),
#' ##   base_100 = TRUE,
#' ##   year_ref_base_100 = 2015,
#' ##   compare = TRUE,
#' ##   var_exporter = "exporter",
#' ##   exporter_ref = "FRA",
#' ##   na.rm = TRUE
#' ## )
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' [.filter_baci()] For more informations concerning the filtering of data inside the function.
#' [.export_data()] For more informations concerning the export of the data inside the function.
#' 
#' @export
aggregate_compare <- function(baci, years = NULL, codes = NULL,
                    export_countries = NULL, import_countries = NULL,
                    method = c("mean", "median", "weighted_mean", "weighted_median"),
                    var, var_aggregation = c("t", "k", "exporter"),
                    var_temporal = "t", var_weight = NULL, fixed_weight = FALSE, 
                    year_ref_fixed_weight = NULL,
                    var_disaggregation = c("t", "exporter", "importer", "k"),
                    base_100 = FALSE,
                    year_ref_base_100 = NULL, compare = FALSE, var_exporter = NULL,
                    exporter_ref = NULL, na.rm = TRUE, return_output = TRUE,
                    return_arrow = TRUE, path_output = NULL){

  # Check validity of method
  method <- match.arg(method)

  # Check if var is a character and length 1
  tradalyze::.check_character(var, "var")
  tradalyze::.check_length_1(var, "var")

  # Check if var_aggregation is a character
  tradalyze::.check_character(var_aggregation, "var_aggregation")

  # Check if var_temporal is character and length 1
  tradalyze::.check_character(var_temporal, "var_temporal")
  tradalyze::.check_length_1(var_temporal, "var_temporal")

  # Check if fixed weight is logical and length 1
  tradalyze::.check_logical(fixed_weight, "fixed_weight")
  tradalyze::.check_length_1(fixed_weight, "fixed_weight")

  # Check if base_100 is logical and length 1
  tradalyze::.check_logical(base_100 ,"base_100")
  tradalyze::.check_length_1(base_100, "base_100")

  if (base_100 == TRUE){
    # Check if year_ref_base_100 is numeric and length 1
    tradalyze::.check_numeric(year_ref_base_100, "year_ref_base_100")
    tradalyze::.check_length_1(year_ref_base_100, "year_ref_base_100")
  }

  # Check if compare is logical and length 1
  tradalyze::.check_logical(compare, "compare")
  tradalyze::.check_length_1(compare, "compare")

  if (compare == TRUE){
    # Check if var_exporter is character and length 1
    tradalyze::.check_character(var_exporter, "var_exporter")
    tradalyze::.check_length_1(var_exporter, "var_exporter")

    # Check if exporter_ref is character and length 1
    tradalyze::.check_character(exporter_ref, "exporter_ref")
    tradalyze::.check_length_1(exporter_ref, "exporter_ref")
  }

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")
  
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
    dplyr::collect()


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

  
  # Transform data into base 100 if wanted
  if (base_100 == TRUE){
    # Isolate values of reference year
    df_year_ref <-
      df_baci |>
      dplyr::filter(!!dplyr::sym(var_temporal) == year_ref_base_100) |>
      dplyr::select(!{{var_temporal}}) |>
      dplyr::rename(!!glue::glue("{var}_year_ref") := .data[[var]])

    # Remove the var_temporal to form the join variables
    # Dont want the year -> allow values of the reference year to be
    # distributed for each observations corresponding
    var_join <-
        var_aggregation[!var_aggregation == var_temporal]

    # Join df_baci and the df with reference values to calculate the base 100
    df_baci <-
      df_baci |>
      dplyr::left_join(
        df_year_ref,
        by = var_join
      ) |>
      dplyr::mutate(
        !!glue::glue("{var}_100") := .data[[var]] / .data[[glue::glue("{var}_year_ref")]] * 100
      )

    
    # Compare base 100 if wanted
    if (compare == TRUE){
      # Isolate values of the reference exporter
      df_exporter_ref <-
        df_baci |>
        dplyr::filter(!!dplyr::sym(var_exporter) == exporter_ref) |>
        dplyr::select(!c({{var_exporter}}, {{var}}, !!dplyr::sym(glue::glue("{var}_year_ref")))) |>
        dplyr::rename(!!glue::glue("{var}_100_exporter_ref") := .data[[glue::glue("{var}_100")]])


    # Remove the var_exporter to form the join variables
    # Dont want the exporter -> allow values of the reference exporter to be
    # distributed for each observations corresponding
      var_join <-
        var_aggregation[!var_aggregation == var_exporter]

      # Join df_baci and the reference values to compare base 100
      df_baci <-
        df_baci |>
        ## dplyr::filter(!!dplyr::sym(var_exporter) != exporter_ref) |>
        dplyr::left_join(
          df_exporter_ref,
          by = var_join
        ) |>
        dplyr::mutate(
          !!glue::glue("{var}_100_diff") := .data[[glue::glue("{var}_100")]] / .data[[glue::glue("{var}_100_exporter_ref")]]
        )
    }
  }


  # Export data
  tradalyze::.export_data(
    data = df_baci,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = TRUE,
    collect = TRUE
  )
}

