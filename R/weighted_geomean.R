#' Calcul une moyenne géométrique pondérée.
#'
#' @param x Un vecteur de données numériques.
#' @param w Un vecteur de poids.
#' @param ... Arguments supplémentaires à passer à la fonction `prod`.
#' @source rogiersbart with his RTOOLZ package : https://rdrr.io/github/rogiersbart/rtoolz/man/weighted.geomean.html
#'
#' @return La moyenne géométrique pondérée.
#' @export
#'
#' @examples # Pas d'exemple.
weighted_geomean <- function(x, w, ...){
  return(prod(x^w, ...)^(1/sum(w)))
}
