#' @title
#' Estimation de la qualité d'un produit à partir d'une équation à la
#' Khandelwal et al (2013)
#'
#' @description
#' Estimation de la qualité d'un produit à partir d'une équation à la
#' Khandelwal et al (2013).
#'
#' @details
#' ## Equation estimée
#' Cette fonction permet d'estimer économétriquement la 'qualité', où plus
#' précisément la compétitivité hors prix d'un produit en se basant sur une
#' modélisation proposée par Khandelwal et al (2013) et modifiée par
#' Bas et al (2015) afin de pouvoir être utilisée avec les données de BACI.
#'
#' Cette modélisation se base sur une fonction de demande qui dépend du prix,
#' de la qualité, du niveau général des prix et de la richesse du pays de
#' destination. La "qualité" d'un produit va être définie comme étant toutes
#' les caractéristiques qui vont faire augmenter la demande d'un produit pour
#' un même prix.
#'
#' Econométriquement, elle est estimée de la façon suivante :
#'
#' \eqn{X_{ijkt} + \sigma_{k} p_{ijkt} = \beta PIB_{it} + \lambda D_{ij} + \alpha_{kjt} + \epsilon_{ijkt}}
#'
#' Cette équation met en relation la quantité échangée d'un produit k, une
#' année t entre l'exportateur i et l'importateur j multipliée par le prix
#' de ce produit (flux) lequel est multiplié par son élasticité prix (la
#' demande décroit en fonction du prix) avec le PIB du pays d'origine, des
#' variables de gravité et un effet fixe produit-importateur-année.
#'
#' Le logarithme de la compétitivité hors-prix est défini comme le résidu de
#' cette relation normalisé par l'élasticité prix du produit correspondant
#' \eqn{\frac{\epsilon_{ijkt}}{\sigma_{k} - 1}}. Le niveau de la compétitivité
#' hors prix est obtenu en prenant l'exponentielle. Le résultat en niveau est
#' le résultat renvoyé par cette fonction. 
#'
#' ## Utilisation des variables
#' Cette fonction utilise la fonction \code{\link[fixest]{feols}} du du package
#' \pkg{fixest} afin d'estimer la régression.
#'
#' Pour indiquer les variables utilisées pour la régression, l'utilisateur
#' peut simplement indiquer les noms des variables dans un vecteur de chaînes de
#' caractères. Il est
#' également possible de simplement passer une chaîne de caractères
#' contenant les relations entre les variables. Si un vecteur de chaînes de
#' caractères est passé, les relations entre les variables seront simplement
#' additionnées. Par exemple : `x_var = c("var1", "var2")` est équivalent à
#' `x_var = "var1 + var2"`. Pour des formes fonctionnelles plus complexes,
#' il faut utiliser la deuxième option. Par exemple pour ajouter un terme
#' d'interaction : `x_var = "var1 * var2"`.
#'
#' Comme dans le package \pkg{fixest}, il est possible d'ajouter des effets
#' fixes combinés en utilisant le symbole `^` entre les noms des variables
#' à effet fixes. Par exemple : `fe_var = c("var1", "var2")` est équivalent à
#' `fe_var = "var1 + var2"`. Ce qui donne deux effets fixes. Pour avoir un seul
#' effet fixe combiné, il faut faire : `fe_var = "var1 ^ var2"`.
#'
#' Enfin il est possible d'estimer plusieurs régression simultanément. Pour
#' cela ilf aut utiliser les fonctions stepwise (voir documentation de
#' \code{\link[fixest]{feols}}). Par exemple :
#' `var_x = "var1 + sw(var2, var3)"` permet d'estimer deux régressions, une
#' avec `var1` et `var2` et une autre avec `var1` et `var3`.
#'
#'
#'
#'
#' @param data_reg Les données à utiliser pour l'estimation. Peut être un
#' chemin vers un dossier parquet, un dataframe ou un objet arrow. Il est
#' recommandé d'utiliser la fonction \link{create_quality_df} pour obtenir
#' les données nécessaires.
#' @param reg_formula Objet formula indiquant la formule à utiliser pour
#' l'équation de régression. Si ce paramètre est renseigné, les paramètres
#' `y_var, `x_var  et `fe_var` ne seront pas pris en compte. 
#' @param y_var La variable dépendante de l'équation de régression.
#' @param x_var Les variables indépendantes de l'équation de régression.
#' @param fe_var Les variables pour les effets fixes de l'équation de régression.
#' @param path_latex_output Le chemin où sauvegarder le tableau en format LaTeX.
#' Par défaut, NULL.
#' @param title_latex Le titre du tableau en format LaTeX. Par défaut, NULL.
#' @param label_latex Le label du tableau en format LaTeX. Par défaut, NULL.
#' @param print_reg_output Afficher les résultats de la régression si TRUE.
#' @param return_output Retourner les résultats de la régression ainsi que
#' les données contenant la qualité si TRUE. Les résultats sont retorunés
#' sous forme de liste avec lm contenant la régression et data_reg contenant
#' les données.
#'
#' @return Les résultats de la régression ainsi que les données contenant la
#' compétitivité hors-prix si return_output = TRUE.
#' Les données de la compétitivité hors-prix renvoyées sont les données en
#' niveau (mise à l'exponentielle).
#' @export
#'
#' @examples # Pas d'exemple
#' @source [Khandelwal, A. K., Schott, P. K., & Wei, S. J. (2013). Trade liberalization and embedded institutional reform: Evidence from Chinese exporters. American Economic Review, 103(6), 2169-2195.](https://www.aeaweb.org/articles?id=10.1257/aer.103.6.2169)
#' @source [Bas, M., Fontagné, L., Martin, P., & Mayer, T. (2015). À la recherche des parts de marché perdues (Research Report No. 2015–23). Conseil d’Analyse Economique.](https://www.cairn.info/revue-notes-du-conseil-d-analyse-economique-2015-4-page-1.htm)
#'
khandelwal_quality_eq <- function(data_reg, reg_formula = NULL, y_var = NULL,
                                  x_var = NULL, fe_var = NULL,
                                  path_latex_output = NULL, title_latex = NULL,
                                  label_latex = NULL, print_reg_output = TRUE,
                                  return_output = TRUE){
  
  # Ouvrir les données de data_reg
  if (is.character(data_reg) == TRUE){
    # Ouvrir les données depuis un dossier parquet
    df_data_reg <-
      data_reg |>
      arrow::open_dataset() |>
      dplyr::collect()
  }
  else if (is.data.frame(data_reg) == TRUE){
    # Ouvrir les données depuis un dataframe : rien faire
    df_data_reg <- data_reg
  }
  else{
    # Ouvrir les données depuis format arrow : passage en format R
    df_data_reg <-
      data_reg |>
      dplyr::collect()
  }

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
