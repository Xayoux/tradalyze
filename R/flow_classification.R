#' @title Perform flow classification from Fontagne et al 1997
#'
#' @param df_baci BACI dataframe (arrow format)
#' @param alpha_H Numeric : Threshold for high variety
#' @param alpha_L Numeric : Threshold for low variety
#' @param var_weighting Character : name of the variable to perform the ponderation
#' @param na.rm Logical : Exclude NA or not
#' @return BACI dataframe with Fontagne et al 1997 classification
classification_fontagne_1997 <- function(df_baci, alpha_H, alpha_L,
                                         var_weighting, na.rm){
  # Check if matrixStats is installed
  rlang::check_installed("matrixStats", reason = "\n\nMandatory to calculate the weighted median.")

  # Check if alpha_H is a numeric and length 1
  tradalyze::.check_numeric(alpha_H, "alpha_H")
  tradalyze::.check_length_1(alpha_H, "alpha_H")

  # Check if alpha_L is a numeric and length 1
  tradalyze::.check_numeric(alpha_L, "alpha_L")
  tradalyze::.check_length_1(alpha_L, "alpha_L")

  # Check if `var_weighting` is a character and length 1
  tradalyze::.check_character(var_weighting, "var_weighting")
  tradalyze::.check_length_1(var_weighting, "var_weighting")

  # Check if `na.rm` is a logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")


  # Perform the classification
  df_baci <-
    df_baci  |>
    dplyr::mutate(
      uv = v / q,
      alpha_H = alpha_H,
      alpha_L = alpha_L
    )  |>
    dplyr::collect()

  # Check if columns are present in df_baci
  tradalyze::.check_var_exist(df_baci, "baci", c(var_weighting, "v", "q", "t", "k"))

  df_baci <-
    df_baci |>
    dplyr::mutate(
      .by = c(t, k),
      med_ref_t_k = matrixStats::weightedMedian(uv, w = !!dplyr::sym(var_weighting), na.rm = na.rm)
    )  |>
    dplyr::mutate(
      fontagne_1997 =
        dplyr::case_when(
          uv > alpha_H * med_ref_t_k ~ "H",
          uv < (1 / (alpha_L)) * med_ref_t_k ~ "L",
          uv > (1 / (alpha_L)) * med_ref_t_k &  uv < alpha_H * med_ref_t_k ~ "M"
        ),
      var_weighting = var_weighting
    )
  return(df_baci)
}


#' @title Perform flow classification from Gaulier et al 2006
#'
#' @param df_baci BACI dataframe (arrow format)
#' @param weight Logical : Weighted quantiles not 
#' @param var_weighting : Variable to perform weighted quantile if weight is TRUE
#' @param na.rm Exclude NA or not
#' @return BACI dataframe with Gaulier et al 2006 classification
classification_gaulier_2006 <- function(df_baci, weight, var_weighting = NULL,
                                        na.rm){
  
  # Check if needed packages are installed
  if (weight == TRUE){
    rlang::check_installed("modi", reason = "\n\nMandatory for calculation of weighted quantiles.")
  } 

  # Check if weight is logical and length 1
  tradalyze::.check_logical(weight, "weight")
  tradalyze::.check_length_1(weight, "weight")

  # Check if var_weighting is NULL or character and length 1
  tradalyze::.check_null_character(var_weighting, "var_weighting")
  tradalyze::.check_length_1(var_weighting, "var_weighting")


  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")  

  # Perform the classification
  df_baci <-
    df_baci  |>
    dplyr::mutate(
      uv = v / q
    ) |>
    dplyr::collect()

  # Check if columns are present in df_baci
  tradalyze::.check_var_exist(df_baci, "baci", c(var_weighting, "v", "q", "t", "k"))

  df_baci <-
    df_baci |>
    dplyr::mutate(
      .by = c(t, k),
      perc_33 =
        dplyr::case_when(
          weight == TRUE ~  modi::weighted.quantile(uv, !!dplyr::sym(var_weighting), prob = 0.33),
          weight == FALSE ~ stats::quantile(uv, prob = 0.33, na.rm = na.rm)
        ),
      perc_67 = 
        dplyr::case_when(
          weight == TRUE ~  modi::weighted.quantile(uv, !!dplyr::sym(var_weighting), prob = 0.67),
          weight == FALSE ~ stats::quantile(uv, prob = 0.67, na.rm = na.rm)
        ),
      var_weighting = dplyr::if_else(weight == FALSE, NA, var_weighting)
    ) |>
    dplyr::mutate(
      gaulier_2006 =
        dplyr::case_when(
          uv <= perc_33 ~ "L",
          uv >= perc_67 ~ "H",
          uv > perc_33 & uv < perc_67 ~ "M"
        ),
      ponderate = weight
    )

  return(df_baci)
}


#' @title Perform flow classification from Fontagne et al 2007
#'
#' @param df_baci BACI dataframe (arrow format)
#' @param alpha Numeric : Parameter for the threshold
#' @param var_weighting Character :  Variable to perform weighted median
#' @param na.rm Exclude NA or not
#' @return BACI dataframe with fontagne et al 2007 classification
classification_fontagne_2007 <- function(df_baci, alpha, var_weighting, na.rm){
  
  # Check if matrixStats is installed
  rlang::check_installed("matrixStats", reason = "\n\nMandatoty for calculation of weighted median.")

  # Check if alpha is numeric and length 1
  tradalyze::.check_numeric(alpha, "alpha_H")
  tradalyze::.check_length_1(alpha, "alpha_H")

  # Check if alpha > 0
  if (alpha <= 0){
    stop(glue:glue("alpha_H must be a strictly positive number, not {alpha}."))
  }

  # Check if var_weighting is NULL or character and length 1
  tradalyze::.check_null_character(var_weighting, "var_weighting")
  tradalyze::.check_length_1(var_weighting, "var_weighting")

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Perform classification
  df_baci <-
    df_baci  |>
    dplyr::mutate(
      alpha = alpha,
      uv = v / q
    ) |>
    dplyr::collect()

  tradalyze::.check_var_exist(df_baci, "baci", c(var_weighting, "v", "q", "t", "k"))

  df_baci <-
    df_baci  |>
    dplyr::mutate(
      .by = c(t, k),
      med_ref_t_k = matrixStats::weightedMedian(uv, w = !!dplyr::sym(var_weighting), na.rm = na.rm)
    )  |>
    # Find th share of each flow that belong to each variety
    # If 0 : This flow has no share in this variety
    dplyr::mutate(
      r = uv / med_ref_t_k,
      share_L =
        dplyr::case_when(
          r < 1 ~ 1 - (r ^ alpha),
          r >= 1 ~ 0 # Not using .default to make NA is r is NA
        ),
      share_M =
        dplyr::case_when(
          r < 1 ~ r ^ alpha,
          r > 1 ~ 1 / (r ^ alpha),
          r == 1 ~ 1
        ),
      share_H =
        dplyr::case_when(
          r > 1 ~ 1 - (1 / (r ^ alpha)),
          r <= 1 ~ 0
        ),
      var_weighting = var_weighting,
      # Calculate the value in each variety
      v_L = v * share_L,
      v_M = v * share_M,
      v_H = v * share_H
    )

  return(df_baci)
}


#' @title Calculation of the Weighted Geometric Mean
#'
#' @param x Numerical vector : data
#' @param w Numerical vector : weight
#' @param ... Additional arguments
#' @source rogiersbart with his RTOOLZ package : https://rdrr.io/github/rogiersbart/rtoolz/man/weighted.geomean.html
#' @return Weighted geometric mean
weighted_geomean <- function(x, w, ...){
  return(prod(x^w, ...)^(1/sum(w, ...)))
}


#' @title Perform Flow Classification from Berthou & Emlinger 2011
#'
#' @param df_baci BACI dataframe (arrow format)
#' @param var_weighting  Variable to perform weighted geometric mean
#' @param na.rm Exclude NA or not
#' @return Berthou 2011 classification
classification_berthou_2011 <- function(df_baci, var_weighting, na.rm){

  # Check if var_weighting is NULL or character and length 1
  tradalyze::.check_null_character(var_weighting, "var_weighting")
  tradalyze::.check_length_1(var_weighting, "var_weighting")

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Perform classification
  df_baci <-
    df_baci |>
    dplyr::mutate(
      uv = v / q
    ) |>
    dplyr::collect()

  # Check if columns are present in df_baci
  tradalyze::.check_var_exist(df_baci, "baci", c(var_weighting, "v", "q", "t", "k" , "j"))

  df_baci <-
    df_baci |>
    dplyr::mutate(
      .by = c(t, k, j),
      weight_share = .data[[var_weighting]] / sum(.data[[var_weighting]], na.rm = na.rm),
      geomean_ref_t_k_j = weighted_geomean(uv, weight_share, na.rm = na.rm)
    ) |>
    dplyr::mutate(
      berthou_2011 =
        dplyr::case_when(
          uv > geomean_ref_t_k_j ~ "H",
          uv <= geomean_ref_t_k_j ~ "L"
        ),
      var_weighting = var_weighting
    ) |>
    dplyr::select(-weight_share)

  return(df_baci)
}







#' @title
#' Variety Classification on BACI Data
#'
#' @description
#' Perform variety classification on the
#' [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
#' database. 4 methods are available to classify each flow into a variety:
#' - Fontagné, Freudenberg and Péridy (1997)
#' - Gaulier, Lemoine and Ünal-Kesenci (2006)
#' - Fontagné, Gaulier and Zignago (2007)
#' - Berthou and Emlinger (2011)
#'
#' @details
#' # Classification methods
#' ## Fontagné, Freudenberg and Péridy (1997)
#' ### Explications
#' This method (available with `method = "fontagne_1997"`) classifies each trade
#' flow into a variety : high (H), medium (M) or low (L). For each trade flow,
#' the unit value (\eqn{v / q}) is calculated for each trade flow. For each
#' group year-product, the weighted median is calculated (usually weighted by
#' the value of the trade flow, but you can choose your variable with
#' `var_weighting`). To classify each flow into a variety, we
#' apply the following rule:
#' - High if : \eqn{uv > (1 + \alpha_H) × median}
#' 
#' - Low if : \eqn{uv < (1 / \alpha_L) × median}
#' 
#' - Medium otherwise.
#'
#' Generally `alpha_H` (\eqn{\alpha_H}) and `alpha_L` (\eqn{\alpha_L}) are set
#' to 1.15.
#'
#' ### requirement
#' To performing this method, the following parameters must be used : `baci`,
#' `method = "fontagne_1997"`, `alpha_H`, `alpha_L` (by default is equal to
#' `alpha_H`), `var_weighting`, `na.rm`.
#'
#' Your BACI data must have at minimum the following variables :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : the value of the trade flow}
#'   \item{q}{Numeric : the quantity of the trade flow}
#' }
#' 
#' It is best to have your trade flows by countries
#' (ie : not aggregated by regions).
#'
#' ### Return
#' This method return at minimum the following variables :
#'
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : Value of the flow}
#'   \item{q}{Numeric : Quantity of the flow}
#'   \item{uv}{Numeric : Unit value of the flow}
#'   \item{med_ref_t_k}{Numeric : Weighted median for each group t-k}
#'   \item{fontagne_1997}{Character : Variety of the flow (H, M or L)}
#'   \item{var_weighting}{Character : the variable used for the weigthingmedian}
#'   \item{alpha_H}{Numeric : \eqn{\alpha_H}, threshold}
#'   \item{alpha_L}{Numeric : \eqn{\alpha_L}, threshold}
#' }
#'
#' ## Gaulier, Lemoine and Ünal-Kesenci (2006)
#' ### Explications
#' This method (available with `method = "gaulier_2006"`) classifies each trade
#' flow into a variety : high (H), medium (M) or low (L). For each trade flow,
#' the unit value (\eqn{v / q}) is calculated. For each group year-product, the
#' weighted (or not, depending on the `weight` parameter) 33th and 67th
#' quantiles are calculated. In general value is used to weight the quantile.
#' To classify each trade flow into a variety, we apply the following rule:
#' - High if : \eqn{uv > q_{67}}
#'
#' - Low if : \eqn{uv < q_{33}}
#'
#' - Medium otherwise
#'
#' ### requirement
#' To perform this method, the following parameters must be used : `baci`,
#' `weight`, `method = "gaulier_2006"`, `var_weighting`, `na.rm`.
#'
#' Your BACI data must have at minimum the following variables :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : the value of the trade flow}
#'   \item{q}{Numeric : the quantity of the trade flow}
#' }
#' 
#' It is best to have your flows by countries (ie : not aggregated by regions).
#'
#' ### Return
#' This method return at minimum the following variables :
#'
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#'   \item{uv}{Numeric : Unit value of the trade flow}
#'   \item{perc_33}{Numeric : 33th (Weighted) quantile for each group t-k}
#'   \item{perc_67}{Numeric : 67th (Weighted) quantile for each group t-k}
#'   \item{gaulier_2006}{Character : Variety of the trade flow (H, M or L)}
#'   \item{ponderate}{Logical : TRUE if a weight is applied. FALSE otherwise}
#'   \item{var_weighting}{Character : the variable used for the weigthing quantile
#' or NA is `weight = TRUE`}
#'   \item{alpha_H}{Numeric : \eqn{\alpha_H}, threshold}
#'   \item{alpha_L}{Numeric : \eqn{\alpha_L}, threshold}
#' }
#'
#' ## Fontagné, Gaulier and Zignago (2007)
#' ### Explications
#' This method (available with `method = "fontagne_2007"`) makes it possible
#' to divide a trade flow into two varieties. A trade flow can have its value
#' split between Low (L) variety and Medium (M) variety, or between High (H)
#' variety and Medium (M) variety. A trade flow can also be classified as
#' Medium (M) only.
#' 
#' The unit value (\eqn{v / q}) is calculated for each trade flow. For each
#' group of year-product, the weighted median is calculated. In general, we
#' use the value of the trade flow to weight. A ratio \eqn{r = uv/uv_m} for
#' each trade flow is computed. Where \eqn{UV_{m}} is the weighted median for
#' the group year-product. Then a share (\eqn{S}) is calculated for each
#' variety according to the following rules:
#' - if : \eqn{r < 1} : \eqn{S_L = 1 - r^{\alpha}} ; \eqn{S_M = r^{\alpha}}
#'
#' - If : \eqn{r > 1} : \eqn{S_H = 1 - 1/(r^{\alpha})} ; \eqn{S_M = 1/(r^{\alpha})}
#'
#' - If : \eqn{r = 1} : \eqn{S_M = 1}
#'
#' with \eqn{\alpha} an exogeneous parameter fixed with `alpha_H`.
#' The more \eqn{\alpha} is small, the more \eqn{S_M} will be large.
#'
#' Using these share we then calculate the value attributed to each variety
#' simply by multipliying the value of the trade flow and the share of the variety
#' for this trade flow.
#'
#' ### requirement
#' To perform this method, the following parameters must be used : `baci`,
#' `method = "fontagne_2007"`, `alpha_H`, `var_weighting`, `na.rm`.
#'
#' Your BACI data must have at minimum the following variables :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : the value of the trade flow}
#'   \item{q}{Numeric : the quantity of the trade flow}
#' }
#' 
#' It is best to have your trade flows by countries (ie : not aggregated by regions).
#'
#' ### Return
#' This method return at minimum the following variables :
#'
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#'   \item{uv}{Numeric : Unit value of the trade flow}
#'   \item{med_ref_t_k}{Numeric : The weighted median for the group t-k}
#'   \item{r}{Numeric : The ratio between `uv` and `med_ref_t_k`}
#'   \item{share_L}{Numeric : The share attributed to the Low variety}
#'   \item{share_M}{Numeric : The share attributed to the Medium variety}
#'   \item{share_H}{Numeric : The share attributed to the High variety}
#'   \item{v_L}{Numeric : The value of the trade flow attributed to the Low variety}
#'   \item{v_M}{Numeric : The value of the trade flow attributed to the Medium variety}
#'   \item{v_H}{Numeric : The value of the trade flow attributed to the High variety}
#'   \item{var_weighting}{character : The variable used to weight}
#' }
#'
#' ## Berthou and Emlinger (2011)
#' This method (available with `method = "berthou_2011"`) classifies each trade
#' flow into a variety : low (L) or high (H). For each trade flow, the unit
#' value (\eqn{v / q}) is calculated. A weighted geometric mean is then
#' calculated for each year-product-importer group. Each tarde flow is
#' classified according to the following rule:
#' - High if : \eqn{uv > geomean}
#'
#' - Low if : \eqn{uv <= geomean}
#'
#' ### requirement
#' To perform this method, the following parameters must be used : `baci`,
#' `method = "berthou_2011"`, `var_weighting`, `na.rm`.
#'
#' Your BACI data must have at minimum the following variables :
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : the value of the trade flow}
#'   \item{q}{Numeric : the quantity of the trade flow}
#'   \item{j}{Numeric/character : Iso (numeric or character) of the importer}
#' }
#' 
#' It is best to have your tarde flows by countries (ie : not aggregated by regions).
#'
#' ### Return
#' This method return at minimum the following variables :
#'
#' \describe{
#'   \item{k}{Character : code product (or any other product indentifier)}
#'   \item{t}{Numeric : Year}
#'   \item{v}{Numeric : Value of the trade flow}
#'   \item{q}{Numeric : Quantity of the trade flow}
#'   \item{j}{Numeric/character : Iso (numeric or character) of the importer}
#'   \item{uv}{Numeric : Unit value of the trade flow}
#'   \item{geomean_ref_t_k_j}{Numeric : The weighted geometric mean for the group t-k-j}
#'   \item{berthou_2011}{character : The variety of the trade flow High (H) or Low (L)}
#'   \item{var_weighting}{character : The variable used to weight}
#' }
#'
#' # Computation
#' This feature uses [arrow](https://arrow.apache.org/docs/r/) functionalities.
#' However, the computation of the various metrics must be in memory
#' (only this part is in memory). This can take some time depending on your
#' configuration and the size of your data. If the size of the data is too
#' large for your computer, it may crash. It is advisable to reduce the size
#' of your database and run this function several times. 
#'
#' @param method Character indicating the method to be used. it can be
#' `fontagne_1997` (the default), `gaulier_2006`, `fontagne_2007` or
#' `berthou_2011` (see details for more).
#' @param alpha_H Threshold used in `method = "fontagne_1997"` and
#' `method = "fontagne_2007"`.
#' Can be NULL if the selected method don't use it.
#' @param alpha_L Treshold used in `method = "fontagne_1997"`. By default it takes
#' the same value has `alpha_H`. Can be NULL if the selected method don't use it.
#' @param var_weighting A character indicating a numerical variable to be used
#' for weighting median, quantile or geometric mean. Can be NULL if
#' `method = "gaulier_2006"` and `weight = FALSE`.
#' @param na.rm Logical indicating if NA should be removed from the data. By
#' default it is set to TRUE. If FALSE you can obtain NA if NA are presents in
#' your data be carefull. 
#' @param weight Logical indicating whether a weight should be applied. Used
#' only if `method = "gaulier_2006"`.
#' @inheritParams .filter_baci
#' @inheritParams .export_data
#' @inheritParams add_chelem_classification
#' 
#' @return BACI data with appropriate variables depending on the choosen method
#' (see details for more).
#'
#' @source
#'  [Fontagné, L., Freudenberg, M., & Péridy, N. (1997). Trade patterns inside the single market (No. 97-07). Paris: CEPII.](http://cepii.fr/PDF_PUB/wp/1997/wp1997-07.pdf)
#' 
#' [G. Gaulier, F. Lemoine & D. Ünal-Kesenci (2006), “China's Emergence and the Reorganisation of Trade Flows in Asia”, CEPII Working Paper, n° 2006-05, March.](http://www.cepii.fr/PDF_PUB/wp/2006/wp2006-05.pdf)
#' 
#' [L.Fontagné, G.Gaulier & S.Zignago (2007),”Specialisation across Varieties within Products and North-South Competition”,CEPII Working Paper, N°2007-06, May](http://www.cepii.fr/PDF_PUB/wp/2007/wp2007-06.pdf)
#' 
#' [A . Berthou, C . Emlinger (2011), « Les mauvaises performances françaises à l’exportation: La compétitivité prix est - elle coupable ? », La Lettre du CEPII , n°313, Septembre.](http://www.cepii.fr/PDF_PUB/lettre/2011/let313.pdf)
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' [.filter_baci()] For more informations concerning the filter of data inside the function.
#' [.export_data()] For more informations concerning the export of the data inside the function.
#' 
#' @examples
#' ## fontagne_1997 method with both alpha set to 1.15, returned in arrow format
#' ## flow_classification(
#' ##   baci = "baci_folder_parquet",
#' ##   method = "fontagne_1997",
#' ##   alpha_H = 1.15,
#' ##   alpha_L = 1.15,
#' ##   var_weighting = "v",
#' ##   na.rm = TRUE,
#' ##   return_output = TRUE,
#' ##   return_arrow = TRUE
#' ## )
#'
#' ## gaulier_2006 method with weighting, save output in pq format and no return
#' ## flow_classification(
#' ##   baci = "baci-folder-parquet",
#' ##   method = "gaulier_2006",
#' ##   weight = TRUE
#' ##   var_weighting = "v",
#' ##   na.rm = TRUE,
#' ##   return_output = FALSE,
#' ##   return_arrow = FALSE,
#' ##   path_output = "folder-output-gaulier-2006"
#' ## )
#'
#' ## fontagne_2007 method with alpha = 3, save output in csv format and return R dataframe
#' ## flow_classification(
#' ##   baci = "baci-folder-parquet",
#' ##   method = "fontagne_2007",
#' ##   alpha_H = 3,
#' ##   var_weighting = "v",
#' ##   na.rm = TRUE,
#' ##   return_output = TRUE,
#' ##   return_arrow = FALSE,
#' ##   path_output = "file-output-fontagne-2007.csv"
#' ## )
#'
#' ## berthou_2011 method with alpha = 3, save output in pq format and return R dataframe
#' ## flow_classification(
#' ##   baci = "baci-folder-parquet",
#' ##   method = "berthou_2011",
#' ##   var_weighting = "v",
#' ##   na.rm = TRUE,
#' ##   return_output = TRUE,
#' ##   return_arrow = FALSE,
#' ##   path_output = "folder-output-berthou-2011.csv"
#' ## )
#' 
#' @export
flow_classification <- function(baci, years = NULL, codes = NULL,
                                export_countries = NULL, import_countries = NULL,
                                method = c("fontagne_1997", "gaulier_2006",
                                           "fontagne_2007", "berthou_2011"),
                                alpha_H = NULL, alpha_L = alpha_H,
                                var_weighting = NULL, na.rm = TRUE, weight = NULL,
                                return_output = TRUE, return_arrow = TRUE,
                                path_output = NULL){

  # Check if `method` parameter is valid
  method <- match.arg(method)

  # Check the validity of the export parameters
  tradalyze::.export_data(
    data = NULL,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = FALSE,
    collect = NULL
  )

  # Load BACI data
  df_baci <- tradalyze::.load_data(baci)

  # Filter BACI data
  df_baci <-
    tradalyze::.filter_baci(
      df_baci = df_baci,
      years = years,
      codes = codes,
      export_countries = export_countries,
      import_countries = import_countries
    )

  # Use the right classification function
  df_baci <-
    switch(
      method,
      "fontagne_1997" =
        classification_fontagne_1997(
          df_baci = df_baci,
          alpha_H = alpha_H,
          alpha_L = alpha_L,
          var_weighting = var_weighting,
          na.rm = na.rm
        ),
      "gaulier_2006" =
        classification_gaulier_2006(
          df_baci = df_baci,
          weight = weight,
          var_weighting = var_weighting,
          na.rm = na.rm
        ),
      "fontagne_2007" =
        classification_fontagne_2007(
          df_baci = df_baci,
          alpha = alpha_H,
          var_weighting = var_weighting,
          na.rm = na.rm
        ),
      "berthou_2011" =
        classification_berthou_2011(
          df_baci = df_baci,
          var_weighting = var_weighting,
          na.rm = na.rm
        )
    ) # Output is in R dataframe format

  tradalyze::.export_data(
    data = df_baci,
    return_output = return_output,
    return_arrow = return_arrow,
    path_output = path_output,
    eval = TRUE,
    collect = TRUE
  )
}

utils::globalVariables(c("alpha_H", "alpha_L", "med_ref_t_k", "v", "q", "uv",
                         "var_weighting", "t", "k", "perc_33", "perc_67",
                         "fontagne_1997", "gaulier_2006", "ponderate", "alpha",
                         "r", "share_L", "share_M", "share_H", "v_L", "v_M",
                         "v_H", "weight_share", "geomean_ref_t_k_j",
                         "berthou_2001"))

