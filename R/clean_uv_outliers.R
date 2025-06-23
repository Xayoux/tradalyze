#' @title Method Classic for Detecting Outliers
#'
#' @param df_baci BACI dataframe
#' @param alpha_H Threshold for high outliers
#' @param alpha_L Threshold for low outliers
#' @param na.rm Exclude NA or not
#' @return BACI df with outliers Highlighted (R dataframe format)
method_classic <- function(df_baci, alpha_H, alpha_L, na.rm){  
  # Check if alpha_H is numeric and length 1
  tradalyze::.check_numeric(alpha_H, "alpha_H")
  tradalyze::.check_length_1(alpha_H, "alpha_H")

  # Check if alpha_H is >= 0 and <= 1
  if (alpha_H < 0 | alpha_H > 1){
    stop(glue::glue("alpha_H must be between [0,1], not {alpha_H}."))
  }
  
  # Check if alpha_L is numeric and length 1
  tradalyze::.check_numeric(alpha_L, "alpha_L")
  tradalyze::.check_length_1(alpha_L, "alpha_L")

  # Check if alpha_L is >= 0 and <= 1
  if (alpha_L < 0 | alpha_L > 1){
    stop(glue::glue("alpha_L must be between [0,1], not {alpha_L}."))
  }

  # Check if alpha_H > alpha_L
  if (alpha_H < alpha_L){
    stop("alpha_H ({alpha_H}) must be greater than alpha_L ({alpha_L}).")
  }

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Check if columns `t`, `k` are present in `df_baci`
  tradalyze::.check_var_exist(df_baci, "baci", c("t", "k"))

  # Compute outliers
  df_baci <-
    df_baci  |>
    dplyr::mutate(
      # Check if uv is in the upper distribution of group t-k
      .by = c(t, k),
      outlier =
        dplyr::case_when(
          uv >= stats::quantile(uv, alpha_H, na.rm = na.rm) ~ 1,
          uv <= stats::quantile(uv, alpha_L, na.rm = na.rm) ~ -1,
          .default = 0
        )
    )

  return(df_baci)
}

#' @title Method fh13 for Detecting Outliers
#'
#' @param df_baci BACI dataframe
#' @param alpha_H Threshold for high outliers
#' @param alpha_L Threshold for low outliers
#' @param na.rm Exclude NA or not
#' @param rm_temp_var Remove temporary variables or not
#' @return BACI df with outliers Highlighted (R dataframe format)
method_fh13 <- function(df_baci, alpha_H, alpha_L, na.rm, rm_temp_var){
  # Check if alpha_H is numeric and length 1
  tradalyze::.check_numeric(alpha_H, "alpha_H")
  tradalyze::.check_length_1(alpha_H, "alpha_H")

  # Check if alpha_H is >= 0 and <= 1
  if (alpha_H < 0 | alpha_H > 1){
    stop(glue::glue("alpha_H must be between [0,1], not {alpha_H}."))
  }
  
  # Check if alpha_L is numeric and length 1
  tradalyze::.check_numeric(alpha_L, "alpha_L")
  tradalyze::.check_length_1(alpha_L, "alpha_L")

  # Check if alpha_L is >= 0 and <= 1
  if (alpha_L < 0 | alpha_L > 1){
    stop(glue::glue("alpha_L must be between [0,1], not {alpha_L}."))
  }

  # Check if alpha_H > alpha_L
  if (alpha_H < alpha_L){
    stop("alpha_H ({alpha_H}) must be greater than alpha_L ({alpha_L}).")
  }

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Check if rm_temp_var is logical and length 1
  tradalyze::.check_logical(rm_temp_var, "rm_temp_var")
  tradalyze::.check_length_1(rm_temp_var, "rm_temp_var")

  # Check if column `k` is present in `df_baci`
  tradalyze::.check_var_exist(df_baci, "baci", "k")

  # Compute outliers
  df_baci <-
    df_baci |>
    # Compute the diff between the uv and the mean of group k
    dplyr::mutate(
      .by = k,
      mean_diff_k = uv - mean(uv, na.rm = na.rm)
    ) |>
    dplyr::mutate(
      # Check if mean diff is in the upper of lower distribution of the mean diff
      outlier =
        dplyr::case_when(
          mean_diff_k >= stats::quantile(mean_diff, alpha_H, na.rm = na.rm) ~ 1,
          mean_diff_k <= stats::quantile(mean_diff, alpha_L, na.rm = na.rm) ~ -1,
          .default = 0
        )
    )

  # Remove temporary variable if wanted
  if (rm_temp_var == TRUE){
    df_baci <-
      df_baci |>
      dplyr::select(!mean_diff_k)
  }

  return(df_baci)
}


#' @title Method h06 for Detecting Outliers
#'
#' @param df_baci BACI dataframe
#' @param alpha_H Threshold for high outliers
#' @param alpha_L Threshold for low outliers
#' @param na.rm Exclude NA or not
#' @return BACI df with outliers Highlighted (R dataframe format)
method_h06 <- function(df_baci, alpha_H, alpha_L, na.rm){
  # Check if alpha_H is numeric and length 1
  tradalyze::.check_numeric(alpha_H, "alpha_H")
  tradalyze::.check_length_1(alpha_H, "alpha_H")

  # Check if alpha_H is > 0
  if (alpha_H <= 0){
    stop(glue::glue("alpha_H must be a positive number, not {alpha_H}"))
  }
  
  # Check if alpha_L is numeric and length 1
  tradalyze::.check_numeric(alpha_L, "alpha_L")
  tradalyze::.check_length_1(alpha_L, "alpha_L")

  # Check if alpha_L is > 0
  if (alpha_L <= 0){
    stop(glue::glue("alpha_L must be a positive number, not {alpha_L}"))
  }

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Check if column `k`, `i`, `t` is present in `df_baci`
  tradalyze::.check_var_exist(df_baci, "baci", c("k", "i", "t"))
  
  # Compute outliers
  df_baci <-
    df_baci  |>
    dplyr::mutate(
      .by = c(k, i, t),
      outlier =
        dplyr::case_when(
          uv > mean(uv, na.rm = na.rm) * alpha_H ~ 1,
          uv < mean(uv, na.rm = na.rm) / alpha_L ~ -1,
          .default = 0
        )
    ) 

  return(df_baci)
}


#' @title Method sd for Detecting Outliers
#'
#' @param df_baci BACI dataframe
#' @param alpha_H Threshold for high outliers
#' @param alpha_L Threshold for low outliers
#' @param na.rm Exclude NA or not
#' @param rm_temp_var Remove temporary variables or not
#' @return BACI df with outliers Highlighted (R dataframe format)
method_sd <- function(df_baci, alpha_H, alpha_L, na.rm, rm_temp_var){
  # Check if alpha_H is numeric and length 1
  tradalyze::.check_numeric(alpha_H, "alpha_H")
  tradalyze::.check_length_1(alpha_H, "alpha_H")

  # Check if alpha_H is > 0
  if (alpha_H <= 0){
    stop(glue::glue("alpha_H must be a positive number, not {alpha_H}"))
  }
  
  # Check if alpha_L is numeric and length 1
  tradalyze::.check_numeric(alpha_L, "alpha_L")
  tradalyze::.check_length_1(alpha_L, "alpha_L")

    # Check if alpha_L is > 0
  if (alpha_L <= 0){
    stop(glue::glue("alpha_L must be a positive number, not {alpha_L}"))
  }

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Check if rm_temp_var is logical and length 1
  tradalyze::.check_logical(rm_temp_var, "rm_temp_var")
  tradalyze::.check_length_1(rm_temp_var, "rm_temp_var")

  # Check if columns `k`, `t` are present in `df_baci`
  tradalyze::.check_var_exist(df_baci, "baci", c("k", "t"))

  # Perform computation of outliers
  df_baci <-
    df_baci |>
    dplyr::mutate(
      .by = c(k, t),
      mean_diff_k_t = uv - mean(uv, na.rm = na.rm),
      sd_k_t = stats::sd(mean_diff_k_t, na.rm = na.rm),
      outlier =
        dplyr::case_when(
          mean_diff_k_t > alpha_H * sd_k_t ~ 1,
          mean_diff_k_t < -alpha_L * sd_k_t ~ -1,
          .default = 0
        )
    )

  # Remove temporary variables if wanted
  if (rm_temp_var == TRUE){
    df_baci <-
      df_baci  |>
      dplyr::select(!c(mean_diff_k_t, sd_k_t))
  }

  return(df_baci)
}


#' @title Method be11 for Detecting Outliers
#'
#' @param df_baci BACI dataframe
#' @param alpha_H Threshold for high outliers
#' @param alpha_L Threshold for low outliers
#' @param beta Threshold for temporal outliers
#' @param na.rm Exclude NA or not
#' @param rm_temp_var Remove temporary variables or not
#' @return BACI df with outliers Highlighted (R dataframe format)
method_be11 <- function(df_baci, alpha_H, alpha_L, beta, na.rm, rm_temp_var){
  # Check if alpha_H is numeric and length 1
  tradalyze::.check_numeric(alpha_H, "alpha_H")
  tradalyze::.check_length_1(alpha_H, "alpha_H")

  # Check if alpha_H is > 0
  if (alpha_H <= 0){
    stop(glue::glue("alpha_H must be a positive number, not {alpha_H}"))
  }
  
  # Check if alpha_L is numeric and length 1
  tradalyze::.check_numeric(alpha_L, "alpha_L")
  tradalyze::.check_length_1(alpha_L, "alpha_L")

  # Check if alpha_L is > 0
  if (alpha_L<= 0){
    stop(glue::glue("alpha_L must be a positive number, not {alpha_L}"))
  }

  # Check if beta is numeric and length 1
  tradalyze::.check_numeric(beta, "beta")
  tradalyze::.check_length_1(beta, "beta")

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Check if rm_temp_var is logical and length 1
  tradalyze::.check_logical(rm_temp_var, "rm_temp_var")
  tradalyze::.check_length_1(rm_temp_var, "rm_temp_var")

  # Check if columns `i`, `j`, `k`, `t` are present in `df_baci`
  tradalyze::.check_var_exist(df_baci, "baci", c("i", "j", "k", "t"))

  
  # Computation of outliers
  df_baci <-
    df_baci |>
    dplyr::mutate(
      .by = c(i, k, t),
      median_i_k_t = stats::median(uv, na.rm = na.rm)
    ) |>
    dplyr::mutate(
      .by = c(i, j, k),
      lag_i_j_k = dplyr::lag(uv, order_by = t),
      lead_i_j_k = dplyr::lead(uv, order_by = t)
    ) |>
    dplyr::mutate(
      outlier =
        dplyr::case_when(
          # Cross section outliers
          uv > median_i_k_t * alpha_H ~ 1,
          uv < median_i_k_t / alpha_L ~ - 1,
          # Temporal outliers
          uv > lag_i_j_k * beta ~ 2,
          uv > lead_i_j_k * beta ~ 2,
          .default = 0
        )
    )

  # Remove temporary variables if wanted
  if (rm_temp_var == TRUE){
    df_baci <-
      df_baci  |>
      dplyr::select(!c(median_i_k_t, lag_i_j_k, lead_i_j_k))
  }
  
  return(df_baci)
}




# Fonction ----------------------------------------------------------------
#' @title Find and Filter Outliers in BACI database
#'
#' @description
#' Identify outliers based on their unit value in the BACI database. These
#' outliers can be removed from the database. Five methods are available to
#' find outliers (see details for more information):
#' - classic
#'
#' - fh13
#'
#' - h06
#'
#' - sd
#'
#' - be11
#' @details
#' Unit values (\eqn{uv = v / q}) calculated from BACI may contain some
#' significant outliers. The quantity data taken from UN COMTRADE may be
#' subject to some measurement errors. To avoid any bias, it is recommended
#' to remove outliers.
#' 
#' # Methods
#' ## classic
#' ### Explications
#' The "classic" method, available with `method = "classic"`, looks at the
#' distribution of unit values for each group year-product. The unit value
#' of is treated as an outlier with the following rule:
#' - if : \eqn{uv < UV_{\alpha L}}
#'
#' - if : \eqn{uv > UV_{\alpha H}}
#'
#' where `alpha_H` (\eqn{\alpha H}) and `alpha_L` (\eqn{\alpha L}) are the
#' quantiles of the distribution of unit values for each group annual product.
#' `alpha_H` and `alpha_L` must be greater than 0 and less than 1 ;
#' \eqn{UV_{\alpha L}} and \eqn{UV_{\alpha H}} are the value taken by the distribution
#' of unit values for these quantiles.
#'
#' ### Requirement
#' The following parameters are needed to perform this method : `baci`,
#' `alpha_H`, `alpha_L`, `na.rm`, `outliers.rm`.
#' 
#' The following variables are needed to perform this method :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#' }
#'
#' ### Return
#' The following variables are returned
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#'   \item{uv}{Numeric : Unit value of the trade flow}
#'   \item{outlier}{Numeric : 1 if the unit value is an upper outlier, -1 if
#' the unit value is a lower outlier. 0 if the unit value is not an outlier. This
#' variable can be removed with `outliers.rm = TRUE`.}
#' }
#' All others variables present in the data are also returned.
#'
#' ## fh13
#' ### Explications
#' The fh13 method, available with `method = fh13`, is taken from.
#' [Fontagné & Hatte (2013)](https://pse.hal.science/hal-00959394/). This
#' method calculates the difference between the unit value and the mean of the
#' unit values for each product group. The unit value is an outlier if:
#' - \eqn{uv < UV_{\alpha L}}
#'
#' - \eqn{uv > UV_{\alpha H}}
#'
#' where `alpha_H` (\eqn{\alpha H}) and `alpha L` (\eqn{\alpha_L}) are the
#' quantiles of the distribution of the mean differences of the unit values
#' for each product group. `alpha_H` and `alpha_L` must be greater than 0 and
#' less than 1; \eqn{UV_{\alpha L}} and \eqn{UV_{\alpha H}} are the value taken by the
#' distribution of the mean differences for these quantiles.
#'
#'
#' ### Requirement
#' The following parameters are needed to perform this method : `baci`,
#' `alpha_H`, `alpha_L`, `na.rm`, `outliers.rm`, `rm_temp_var`.
#' 
#' The following variables are needed to perform this method :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#' }
#'
#' ### Return
#' The following variables are returned
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#'   \item{uv}{Numeric : Unit value of the trade flow}
#'   \item{outlier}{Numeric : 1 if the unit value is an upper outlier, -1 if
#' the unit value is a lower outlier. 0 if the unit is not an outlier. This
#' variable can be unselected with `outliers.rm = TRUE`.}
#'   \item{mean_diff_k}{Numeric : The difference between the unit value and the
#' mean of unit values for each product group. This variable can be removed
#' with `rm_temp_var = TRUE`.}
#' }
#' 
#' All others variables present in the data are also returned.
#'
#' ## h06
#' ### Explications
## The h06 method, available with `method = "h06"` comes from
## [Hallak (2006)](https://www.sciencedirect.com/science/article/abs/pii/S0022199605000516).
## This method compute the mean of the unit values for each group
## exporter-year-product. The unit value is considered to be an outlier if:
#' - \eqn{uv < UV_{ikt} / \alpha_L}
#'
#' - \eqn{uv > UV_{ikt} * \alpha_H}
#'
#' with `alpha_H` (\eqn{\alpha_H}) and `alpha_L` (\eqn{\alpha_L}) two constant
#' and \eqn{UV_{ikt}} the mean of unit values for each group exporter-year-product.
#'
#' ### Requirement
#' The following parameters are needed to perform this method : `baci`,
#' `alpha_H`, `alpha_L`, `na.rm`, `outliers.rm`.
#' 
#' The following variables are needed to perform this method :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{i}{Numeric/character : Iso numeric or character of the exporter}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#' }
#'
#' ### Return
#' The following variables are returned (at minimum)
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{i}{Numeric/character : Iso numeric or character of the exporter}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#'   \item{uv}{Numeric : Unit value of the trade flow}
#'   \item{outlier}{Numeric : 1 if the unit value is an upper outlier, -1 if
#' the unit value is a lower outlier. 0 if the unit is not an outlier. This
#' variable can be unselected with `outliers.rm = TRUE`}
#' }
#' 
#' All others variables present in the data are also returned.
#'
#' ## sd
#' ### Explications
#' the sd method, available with `method= "sd"`, caclculates the difference
#' between the unit value and the mean of the unit values for each 
#' product-year group. It then calculates the standard deviation of these differences
#' for each product-year group. A unit value
#' is an outlier if:
#' - \eqn{UV_{tk} < sd_{tk} * (-\alpha_L)}
#' 
#' - \eqn{UV_{tk} > sd_{tk} * \alpha_H}
#'
#' with `alpha_H` (\eqn{\alpha_H}) and `alpha_L` (\eqn{\alpha_L}) two constant
#' and \eqn{sd_{tk}} the standard deviation of the distribution of the mean
#' differences for each group year-product and \eqn{UV_{tk}} the difference between the
#' unit value and the mean of unit values for each year-product group.
#'
#' ### Requirement
#' The following parameters are needed to perform this method : `baci`,
#' `alpha_H`, `alpha_L`, `na.rm`, `outliers.rm`, `rm_temp_var`.
#' 
#' The following variables are needed to perform this method :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#' }
#'
#' ### Return
#' The following variables are returned (at minimum)
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#'   \item{uv}{Numeric : Unit value of the trade flow}
#'   \item{outlier}{Numeric : 1 if the unit value is an upper outlier, -1 if
#' the unit value is a lower outlier. 0 if the unit is not an outlier. This
#' variable can be unselected with `outliers.rm = TRUE`}
#'   \item{mean_diff_k_t}{Numeric : The difference between the unit value and the
#' mean of unit values for each product group. This variable can be removed
#' with `rm_temp_var = TRUE`.}
#'   \item{sd_k_t}{Numeric : the standard deviation of `mean_diff_k_t`
#' for each product group. This variable can be removed
#' with `rm_temp_var = TRUE`.}
#' }
#' 
#' All others variables present in the data are also returned.
#'
#' ## be11
#' ### Explications
#' The method be11, available with `method = "be11"`, is taken from
#' [Berthou & Emlinger (2011)](http://www.cepii.fr/PDF_PUB/wp/2011/wp2011-10.pdf).
#' It calculates the median of the unit value distribution for each
#' exporter-product-year group. It then calculates the lag and lead of each
#' unit value (group exporter-importer-product). The unit value is an outlier if:
#' - \eqn{uv < UV_{ikt} / \alpha_L}
#'
#' - \eqn{uv > UV_{ikt} * \alpha_H}
#'
#' for the cross-section outliers. And the unit value will be a temporal outlier if:
#' - \eqn{uv < lag_{ijk} * \beta}
#' 
#' - \eqn{uv > lag_{ijk} * \beta}
#'
#'
#' Where `alpha_H` (\eqn{\alpha_H}) and `alpha_L` (\eqn{\alpha_L}) are two constant ;
#' \eqn{UV_ikt} is the median of the unit value distribution for each group
#' exporter-product-year ; \eqn{\beta} a constant ; \eqn{lag_{ijk}} and
#' \eqn{lead_{ijk}} the lag and lead of the unit value.
#'
#' ### Requirement
#' The following parameters are needed to perform this method : `baci`,
#' `alpha_H`, `alpha_L`, `beta`, `na.rm`, `outliers.rm`, `rm_temp_var`.
#' 
#' The following variables are needed to perform this method :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{i}{Numeric/character : Iso numeric or character of the exporter}
#'   \item{j}{Numeric/character : Iso numeric or character of the importer}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#' }
#'
#' ### Return
#' The following variables are returned
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{i}{Numeric/character : Iso numeric or character of the exporter}
#'   \item{j}{Numeric/character : Iso numeric or character of the importer}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#'   \item{uv}{Numeric : Unit value of the trade flow}
#'   \item{outlier}{Numeric : 1 if the unit value is an upper outlier, -1 if
#' the unit value is a lower outlier, 2 if the unit value is a temporal outlier.
#' 0 if the unit is not an outlier. This variable can be unselected
#' with `outliers.rm = TRUE`}
#'   \item{median_i_k_t}{Numeric : The median of the unit value distribution
#' for each exporter-product-year group. This variable can be removed
#' with `rm_temp_var = TRUE`.}
#'   \item{lag_i_j_k}{Numeric : The lag of the unit value.
#' This variable can be removed with `rm_temp_var = TRUE`.}
#'   \item{lead_i_j_k}{Numeric : The lead of the unit value.
#' This variable can be removed with `rm_temp_var = TRUE`.}
#' }
#' 
#' All others variables present in the data are also returned.
#'
#' # Computation
#' This feature uses [arrow](https://arrow.apache.org/docs/r/) functionalities.
#' However, the computation of the various metrics must be in memory
#' (only this part is in memory). This can take some time depending on your
#' configuration and the size of your data. If the size of the data is too
#' large for your computer, it may crash. It is advisable to reduce the size
#' of your database and run this function several times.
#'
#' @param method Method to determine outliers. It can be `classic` (the default),
#' `fh13`, `h06`, `sd`, `be11` (see details for more informations).
#' @param alpha_H Numeric : Parameter to determine the upper outlier. Its
#' meaning depends on the method chosen
#' @param alpha_L Numeric : Parameter to determine the lower outlier. Its
#' meaning depends on the method chosen
#' @param beta Numeric : Parameter to determine temporal outliers in the `be11` method.
#' It can be NULL if another method is selected.
#' @param outliers.rm Logical indicating whether outliers should be removed from the
#' data or not (the default).
#' @param rm_temp_var Logical indicating whether temporary variables should be
#' removed from the data or not (the default) (see details for more informations
#' about these variables).
#' @param na.rm Logical indicating whether NA should be remove from the data for
#' the computation. By default it is set to TRUE. If FALSE you can obtain NA
#' if NA are presents in your data be carefull. 
#' @inheritParams .filter_baci
#' @inheritParams .export_data
#' @inheritParams add_chelem_classification
#'
#' @return BACI data with appropriate variables depending on the method.
#' Outliers can be removed or not.
#'
#' @examples
#' ## find outliers with classic method. Keep outliers return arrow object
#' ## clean_uv_outliers(
#' ##   baci = "baci-folder-parquet",
#' ##   method = "classic",
#' ##   alpha_H = 0.95,
#' ##   alpha_L = 0.05,
#' ##   na.rm = TRUE,
#' ##   outliers.rm = FALSE,
#' ##   return_output = TRUE,
#' ##   return_arrow = TRUE
#' ## )
#'
#' ## find outliers with fh13 method. Don't keep outliers return R dataframe
#' ## object. Keep temporary variables
#' ## clean_uv_outliers(
#' ##   baci = "baci-folder-parquet",
#' ##   method = "fh13",
#' ##   alpha_H = 0.95,
#' ##   alpha_L = 0.05,
#' ##   na.rm = TRUE,
#' ##   outliers.rm = TRUE,
#' ##   rm_temp_var = FALSE
#' ##   return_output = TRUE,
#' ##   return_arrow = FALSE
#' ## )
#'
#' ## find outliers with h06 method. Don't keep outliers, save in parquet format.
#' ## clean_uv_outliers(
#' ##   baci = "baci-folder-parquet",
#' ##   method = "h06",
#' ##   alpha_H = 5,
#' ##   alpha_L = 5,
#' ##   na.rm = TRUE,
#' ##   outliers.rm = TRUE,
#' ##   return_output = FALSE,
#' ##   return_arrow = FALSE,
#' ##   path_output = "folder-output-parquet"
#' ## )
#'
#' ## find outliers with sd method. Don't keep outliers, save in csv format.
#' ## clean_uv_outliers(
#' ##   baci = "baci-folder-parquet",
#' ##   method = "sd",
#' ##   alpha_H = 3,
#' ##   alpha_L = 3,
#' ##   na.rm = TRUE,
#' ##   outliers.rm = TRUE,
#' ##   rm_temp_var = TRUE
#' ##   return_output = FALSE,
#' ##   return_arrow = FALSE,
#' ##   path_output = "file-output-csv.csv"
#' ## )
#'
#' ## find outliers with be11 method. Don't keep outliers, return arrow format,
#' ## keep only year 2015 to 2017.
#' ## clean_uv_outliers(
#' ##   baci = "baci-folder-parquet",
#' ##   years = 2015:2017,
#' ##   method = "be11",
#' ##   alpha_H = 10,
#' ##   alpha_L = 10,
#' ##   beta = 1000,
#' ##   na.rm = TRUE,
#' ##   outliers.rm = TRUE,
#' ##   rm_temp_var = FALSE,
#' ##   return_output = TRUE,
#' ##   return_arrow = TRUE
#' ## )
#'
#' @source [Lionel Fontagné, Sophie Hatte. European High-End Products in International Competition. 2013.](https://pse.hal.science/hal-00959394/)
#'
#' [Hallak, J. C. (2006). Product quality and the direction of trade. Journal of international Economics, 68(1), 238-265.](https://www.sciencedirect.com/science/article/abs/pii/S0022199605000516)
#' 
#' [Antoine Berthou & Charlotte Emlinger , 2011. "The Trade Unit Values Database," CEPII Working Paper 2011- 10 , April 2011 , CEPII.](http://www.cepii.fr/PDF_PUB/wp/2011/wp2011-10.pdf)
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' [.filter_baci()] For more informations concerning the filter of data inside the function.
#' [.export_data()] For more informations concerning the export of the data inside the function.
#' 
#' @export
clean_uv_outliers <- function(baci, years = NULL, codes = NULL,
                              export_countries = NULL, import_countries = NULL,
                              method = c("classic", "fh13", "h06", "sd", "be11"),
                              alpha_H, alpha_L, beta = NULL, na.rm = TRUE,
                              outliers.rm = FALSE, rm_temp_var = NULL,
                              return_output = TRUE, return_arrow = TRUE,
                              path_output = NULL) {

  # Check if method parameter is valid
  method <- match.arg(method)

  # Check the validity of export parameters
  tradalyze::.export_data(
    data = NULL,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = FALSE,
    collect = NULL
  )

  tradalyze::.check_logical(outliers.rm, "outliers.rm")
  tradalyze::.check_length_1(outliers.rm, "outliers.rm")
  

  # Load the data
  df_baci <- tradalyze::.load_data(baci)

  # Filter data
  df_baci <-
    tradalyze::.filter_baci(
      df_baci = df_baci,
      years = years,
      codes = codes,
      export_countries = export_countries,
      import_countries = import_countries
    ) |>
    dplyr::mutate(
      uv = v / q
    ) |>
    dplyr::collect()

  df_baci <-
    switch(
      method,
      "classic" =
        method_classic(
          df_baci = df_baci,
          alpha_H = alpha_H,
          alpha_L = alpha_L,
          na.rm = na.rm
        ),
      "fh13" =
        method_fh13(
          df_baci = df_baci,
          alpha_H = alpha_H,
          alpha_L = alpha_L,
          na.rm = na.rm,
          rm_temp_var = rm_temp_var
        ),
      "h06" =
        method_h06(
          df_baci = df_baci,
          alpha_H = alpha_H,
          alpha_L = alpha_L,
          na.rm = na.rm
        ),
      "sd" =
        method_sd(
          df_baci = df_baci,
          alpha_H = alpha_H,
          alpha_L = alpha_L,
          na.rm = na.rm,
          rm_temp_var = rm_temp_var
        ),
      "be11" =
        method_be11(
          df_baci = df_baci,
          alpha_H = alpha_H,
          alpha_L = alpha_L,
          na.rm = na.rm,
          rm_temp_var = rm_temp_var
        )
    ) # Output is in R dataframe format
    
  if (outliers.rm == TRUE){
    nrow_base <- nrow(df_baci)
    
    df_baci <-
      df_baci  |>
      dplyr::filter(outlier == 0) |>
      dplyr::select(-outlier)
    
    nrow_rm_outliers <- nrow(df_baci)
    nb_outliers <- nrow_base - nrow_rm_outliers
    message(glue::glue("{nb_outliers} outliers had been removed."))
  }

  # Export output
  tradalyze::.export_data(
    data = df_baci,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = TRUE,
    collect = TRUE
  )
}

utils::globalVariables(c("t", "k", "uv", "outlier", "mean_diff_k",
                         "i", "mean_diff_k_t", "sd_k_t", "median_i_k_t",
                         "lag_i_j_k", "lead_i_j_k"))
