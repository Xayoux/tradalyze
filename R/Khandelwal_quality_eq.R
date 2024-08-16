#' @title Estimate Quality of a Trade Flow/Product (Khandelwal et al 2013)
#'
#' @description
#' Allow to estimate the quality of a product with aggregated commercial
#' data like [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37).
#' The estimate equation can be freely choosen and can be multiple to
#' conduct robustness check. The creation of the data can be made
#' with the function \link{create_quality_df} from this package, and the
#' aggregation by prodict or sector can be made with the function
#' \link{aggregate_compare} from this package.
#'
#' @details
#' ## Estimate equation
#' This function allow to estimate with a regression the "quality"
#' (non-price competitiveness) of a product based on a modelisation
#' proposed by Khandelwal et al (2013) and adapted to aggregated commercial
#' data by Bas et al (2015)
#'
#' This modelisation is based on a demand function that depends on the price,
#' quality, the level of price and the wealth of the destination country.
#' The "quality" is defined as all the characteristics that increase the
#' demand for a same price.
#'
#' It is estimate with the following regression :
#'
#' \eqn{X_{ijkt} + \sigma_{k} p_{ijkt} = \beta PIB_{it} + \lambda D_{ij} + \alpha_{kjt} + \epsilon_{ijkt}}
#'
#' X beeing the quantity of the trade flow; \eqn{\sigma_k} the elasticity of
#' the product ; p the price of the trade flow ; PIB the PIB of the
#' origin country ; D some gravity variables and \eqn{\alpha_{kjt}} a
#' product-importer-year fixed effect.
#'
#' The logarithm of the non-price competitiveness is defined as the residuals
#' of this equation standardized by the elasticities of the product
#' \eqn{\frac{\epsilon_{ijkt}}{\sigma_{k} - 1}}. The level of the "quality" for
#' the flow is obtained by taking the exponential of this. The results of this
#' function is the level of "quality".
#'
#' ## Use of variables
#' This function use the function \code{\link[fixest]{feols}} of \pkg{fixest}
#' to estimate the regression.
#'
#' To indicate the variables used for the regression, the user can
#' simply indicate the names of theses variables in a character vector in the
#' `x_var`, `y_var` and `fe_var` parameters. It is also possible to indicate
#' precisely the relation between the variable. By default it is simply an addition.
#' For example `x_var = c("var1", "var2")` is equivalent to `x_var = "var1 + var2"`.
#' For complex form it can be `x_var = "var1 * var2"`.
#'
#' It is possible to add combined fixed effects with the symbol '^' between
#' the name of the fixed effects variables. For example `fe_var = c("var1", "var2")`
#' is equivalent to `fe_var = "var1 + var2"` whihc means two fixed effects.
#' To have only one combined fixed effect : `fe_var = "var1 ^ var2"`.
#'
#' It is possible to estimate multiple regressions with stepwise functions :
#' see \code{\link[fixest]{feols}}. For example `var_x = "var1 + sw(var2, var3)"`
#' allow to estimate two regressions, one with `var1` and `var2` and the other
#' with `var1` and `var3`.
#'
#' With the parmater `reg_formula`, you can directly give an object formula
#' to estimate the regression. 
#'
#' @param data_reg Data to be used for the regression. Can be a path to a csv file.
#' It can also be a path to a folder containing
#' parquet files. Dataframe and ArrowObject are also accepted. Xlsx files are
#' also accepted, but absolutely not recommended because BACI data are too large
#' and data must be in the first sheet. It is recommended to use the
#' \link{create_quality_df} to obtain these data.
#' @param reg_formula Formula object indicating the formula to be used for
#' the regression. If this parameter is non NULL, `y_var, `x_var  et `fe_var`
#' will not be taking into account.
#' @param y_var Character indicating the name of the dependant variable
#' (classically it should be "demand").
#' @param x_var Character indicating the independant variables
#' (see details for more informations).
#' @param fe_var Character indictaing the fixed effects to be used
#' (See details for more informations).
#' @param path_latex_output Character indicating the path to where the
#' LaTeX table should be saved. If NULL table will not be saved. 
#' @param title_latex Title of the LaTeX Table. By default NULL.
#' @param label_latex Label of the LaTeX table. By default NULL.
#' @param print_reg_output Logical indicating whether the results of the
#' regressions must be displayed (TRUE : the default) or not (FALSE). 
#' @param return_output Logical indicating whether to return the output of
#' the rregression and the data containing the quality (TRUE the default) or not
#' (FALSE). If TRUE results are returned as a list with "lm" containing the
#' regression and "data_reg" the data. 
#'
#' @return Results of the regression and data. Quality data are in level.
#' The following variables are added to the original data :
#' \describe{
#'   \item{epsilon}{Numeric : residuals of the regression. If multiples
#' equations are estimated, it will be followed by the number of the regression
#' (epsilon_1, epsilon_2...)}
#'   \item{quality}{Numeric : Level of the quality. If multiples
#' equations are estimated, it will be followed by the number of the regression
#' (quality_1, quality_2...)}
#' }
#' 
#'
#' @examples
#' ## Estimate the regression and display regression table
#' ## eq <- khandelwal_quality_eq(
#' ##   data_reg = "path-to-parquet-folder",
#' ##   y_var = "demand",
#' ##   x_var = c("gdp_o", "contig", "dist") # variables taken from Gravity,
#' ##   fe_var = "k^importer^t",
#' ##   print_reg_output = TRUE,
#' ##   return_output = TRUE
#' ## )
#' 
#' ## Take the data
#' ## eq$data_reg
#' 
#' ## Take the regression output
#' ## eq$lm
#' 
#' @source
#' [Khandelwal, A. K., Schott, P. K., & Wei, S. J. (2013). Trade liberalization and embedded institutional reform: Evidence from Chinese exporters. American Economic Review, 103(6), 2169-2195.](https://www.aeaweb.org/articles?id=10.1257/aer.103.6.2169)
#'
#' [Bas, M., Fontagné, L., Martin, P., & Mayer, T. (2015). À la recherche des parts de marché perdues (Research Report No. 2015–23). Conseil d’Analyse Economique.](https://www.cairn.info/revue-notes-du-conseil-d-analyse-economique-2015-4-page-1.htm)
#' @export
khandelwal_quality_eq <- function(data_reg, reg_formula = NULL, y_var = NULL,
                                  x_var = NULL, fe_var = NULL,
                                  path_latex_output = NULL, title_latex = NULL,
                                  label_latex = NULL, print_reg_output = TRUE,
                                  return_output = TRUE){

  # Check if fixest is installed
  rlang::check_installed("fixest", reason = "Mandatory to perform the regression.")
  
  df_data_reg <- tradalyze::.load_data(data_reg)

  # Créer la formule "manuellement" si formula = NULL
  if (is.null(reg_formula) == TRUE | !methods::is(reg_formula, "formula")){
    # Si la longeur de x_var est de 1 = écriture sous forme de 'formule'.
    # So > 1 = uniquement noms de variables -> transformation en 'formule'
    # Uniquement sous forme additive
    if(length(x_var > 1)){
      x_var <- paste(x_var, collapse = " + ")
    }

    # Pareil pour fe_var
    if(length(fe_var > 1)){
      fe_var <- paste(fe_var, collapse = " + ")
    }

    # Création de la formule de régression
    reg_formula <-
      stats::formula(
        stringr::str_glue(
          "{y_var} ~ {x_var} | {fe_var}"
        )
      )
  }  

  # Estimation de la régression OLS
  lm <- fixest::feols(
    reg_formula, data = data_reg
  )


  # Afficher les résultats si voulus
  if(print_reg_output == TRUE){
    print(fixest::etable(lm, se.below = TRUE))
  }


  # Pour chaque équation estimée, stocker les résidus et calculer la qualité
  if (methods::is(lm, "fixest_multi")){
    for (i in 1:length(lm)){
      colname_epsilon <- stringr::str_glue("epsilon_{i}")
      colname_quality <- stringr::str_glue("quality_{i}")

      data_reg <-
        data_reg |>
        dplyr::mutate(
          !!colname_epsilon := lm[[i]]$residuals,
          !!colname_quality := exp(lm[[i]]$residuals / (sigma - 1))
        )
    }
  }
  else {
    data_reg <-
      data_reg |>
      dplyr::mutate(
        epsilon = lm$residuals,
        quality = exp(epsilon / (sigma - 1))
      )
  }


  if (!is.null(path_latex_output)){
    fixest::etable(
      lm,
      se.below = TRUE,
      tex = TRUE,
      title = title_latex,
      label = label_latex,
      file = path_latex_output
    )
  }

  if (return_output == TRUE){
    return(
      list(lm = lm, data_reg = data_reg)
    )
  }
}
