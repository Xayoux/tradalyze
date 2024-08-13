#' @title Sum values in the Desired Variable
#'
#' @param df Dataframe
#' @param var_aggregate Character vector of variables to be used for
#' the aggregation of the sum
#' @param var_share_indiv Variable to sum
#' @param na.rm Exlude NA or not
#' @return Dataframe with variable sum at the desired level
sum_variable <- function(df, var_aggregate, var_share_indiv, na.rm){
  df_summarized <-
    df |>
    dplyr::summarize(
      .by = var_aggregate,
      !!var_share_indiv := sum(.data[[var_share_indiv]], na.rm = na.rm)
    )

  return(df_summarized)
}


#' @title Compute the Market Share for the Desired Variable
#'
#' @param df Dataframe
#' @param var_aggregate Character vector of variables to be used
#' for the aggregation of the market share
#' @param var_share_indiv Variable on which to compute the market share
#' @return Dataframe with the market share computed
market_share_variable <- function(df, var_aggregate, var_share_indiv){
  df_market_share <-
    df |>
    dplyr::mutate(
      .by = var_aggregate,
      !!glue::glue("market_share_{var_share_indiv}") := .data[[var_share_indiv]] / sum(.data[[var_share_indiv]]) * 100
    )

  return(df_market_share)
}

#' @title
#' Compute Market Share
#'
#' @description
#' Use the data to calculate the market share of the desired variables. The
#' level of of aggregation is freely selectable. 
#'
#' @details
#' This function aggregate (sum) trade flow for each `var_aggregate` group for the
#' desired variables : `var_share`. Then shares are computed for each variables
#' in `var_share` with level of aggregation corresponding to `var_aggregate` minus
#' the last variable store in `var_aggregate`. Market shares are returned in
#' percentage. 
#'
#' This feature uses [arrow](https://arrow.apache.org/docs/r/) functionalities
#' to load and filter the data. However, the computation of the must be in memory.
#' This can take some time depending on your
#' configuration and the size of your data. If the size of the data is too
#' large for your computer, it may crash. It is advisable to reduce the size
#' of your database and run this function several times.
#'
#' @param var_aggregate A character vector specifying the variables used to
#' aggregate the data in the calculation of market shares.
#'
#' This parameter determines how the market share will be calculated by defining
#' the group of variables that will be used for aggregation. 
#'
#' - **Market Power** When there is only exporter or importer
#' (e.g, `c("t", "k", "exporter")`), the function calculates the market share
#' of the exporter for the product k in year t. This could reveal that
#' Country A export z% of product k in year t.
#'
#' If the aggregation is more detailed, the order of the variables in
#' `var_aggregate` is crucial as it changes the
#' perspective of the market share analysis:
#'
#' - **Exporter Perspective**: When the exporter is listed first
#' (e.g., `c("t", "k", "exporter", "importer")`), the function calculates
#' the market share of the exporter within each importer. This allows you to
#' determine the proportion of a product exported
#' from a particular country to different importing countries. For example,
#' using this configuration could reveal that country A exports x%
#' of its product k to country B.
#'
#' - **Importer Perspective**: When the importer is listed first
#' (e.g., `c("t", "k", "importer", "exporter")`), the function calculates
#' the market share of the importer for products coming from each exporter.
#' This configuration helps in understanding the share of imports from specific
#' exporters into a particular importing country. For instance, this setup
#' could show that Country B imports y% of its product k from Country C.
#'
#' By adjusting the variables and their order in `var_aggregate`, you can
#' flexibly analyze market shares from different perspectives, providing
#' valuable insights into trade dynamics based on the specified aggregation.
#' @param var_share A character vector indicating variables to be used to
#' compute the market share. These variables must be numeric. 
#' 
#' @inheritParams .filter_baci
#' @inheritParams .export_data
#' @inheritParams add_chelem_classification
#' @inheritParams clean_uv_outliers
#'
#' @return Dataframe with the following variables :
#' \describe{
#'   \item{var_aggregate}{All the variables in var_aggregate}
#'   \item{var_share}{All the variables in var_share}
#'   \item{market_share_var_share}{the MArket share of each variable in
#' `var_share`. If `var_share = "v"`, the name will be `market_share_v`.}
#' }
#'
#' @examples # Pas d'exemple.
#' ## Share of v of the exporter for product 280530 and years 2020 to 2022
#' ## market_share(
#' ##   baci = "folder-baci-parquet",,
#' ##   years = 2020:2022,
#' ##   codes = "280530",
#' ##   var_aggregate = c("t", "k", "exporter"), # Exporter in last is very important
#' ##   var_share = "v",
#' ##   return_arrow = FALSE  
#' ## )
#'
#' ## Proportion of v and q exported from exporters to each importer
#' ## Country A export x% of product k to Country B
#' ## market_share(
#' ##   baci = "folder-baci-parquet",,
#' ##   years = 2020:2022,
#' ##   codes = "280530",
#' ##   var_aggregate = c("t", "k", "exporter", "importer"), # Importer in last is very important
#' ##   var_share = c("v", "q"),
#' ##   return_arrow = FALSE  
#' ## )
#'
#' ## Proportion of v and q imported from each exporters to an importer
#' ## Country B import x% of its product k from Country B
#' ## market_share(
#' ##   baci = "folder-baci-parquet",,
#' ##   years = 2020:2022,
#' ##   codes = "280530",
#' ##   var_aggregate = c("t", "k", "importer", "exporter"), # Exporter in last is very important
#' ##   var_share = c("v", "q"),
#' ##   return_arrow = FALSE  
#' ## )
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' [.filter_baci()] For more informations concerning the filter of data inside the function.
#' [.export_data()] For more informations concerning the export of the data inside the function.
#'
#' @export
market_share <- function(baci, years = NULL, codes = NULL,
                         export_countries = NULL, import_countries = NULL,
                         var_aggregate, var_share, na.rm = TRUE,
                         return_output = TRUE,
                         return_arrow = TRUE, path_output = NULL){

  # Check if export parameters are valid
  tradalyze::.export_data(
    data = NULL,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = FALSE,
    collect = NULL
  )

  # Check if var_aggregate is character
  tradalyze::.check_character(var_aggregate, "var_aggregate")

  # Check if var_share is character
  tradalyze::.check_character(var_share, "var_share")

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")
  
  # The second aggregation (to compute market share) : the last variable is removed
  var_aggregate_2 <- var_aggregate[-length(var_aggregate)]

  # Load baci data
  df_baci <- tradalyze::.load_data(baci)

  # Filter baci data
  df_baci <-
    tradalyze::.filter_baci(
      df_baci = df_baci,
      years = years,
      codes = codes,
      export_countries = export_countries,
      import_countries = import_countries
    )  |>
    dplyr::collect()

  # Check if variables are in the df
  tradalyze::.check_var_exist(df_baci, "baci", c(var_aggregate, var_share))
  
  # Compute the sum of each variable wanted for the share
  df_baci <-
    purrr::map(
      var_share,
      \(var_share) sum_variable(
        df = df_baci,
        var_aggregate = var_aggregate,
        var_share_indiv = var_share,
        na.rm = na.rm
      )
    ) |>
    purrr::reduce(dplyr::full_join, by = var_aggregate) # full_join df in the list

  # Compute the marjet-share for each variable wanted
  df_baci <-
    purrr::map(
      var_share,
      \(var_share) market_share_variable(
        df = df_baci,
        var_aggregate = var_aggregate_2,
        var_share_indiv = var_share
      )
    ) |>
    purrr::reduce(dplyr::full_join, by = c(var_aggregate, var_share))

  
  # Export the data
  tradalyze::.export_data(
    data = df_baci,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = TRUE,
    collect = TRUE
  )
}

