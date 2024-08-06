# chelem_classification ---------------------------------------------------
#' Geographic classification used by the database
#' [CHELEM] (<http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=17>).
#'
#' @format ## `classification_chelem`
#' A dataframe with 196 rows and 3 columns :
#' \describe{
#'   \item{iso_region}{Str : Character iso code 3 use for the region.}
#'   \item{name_region}{Str : The name of the region.}
#'   \item{iso_country}{Str : Character iso code 3 use for the country}
#' }
#' @source [CHELEM classificiation](<http://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=17>). See :
#' [de Saint Vaulry, A. (2008), “Base de données CHELEM - Commerce international du CEPII”,  Document de travail du CEPII, N°2008-09](http://www.cepii.fr/cepii/fr/publications/wp/abstract.asp?nodoc=1081)
"chelem_classification"


# ptle -------------------------------------------------------------------
#' Elasticités du commerce pour les produits HS6 : nomenclature HS2007
#'
#' @format ## `ptle`
#' Un dataframe avec 5050 lignes et 4 colonnes :
#' \describe{
#'   \item{HS6}{Codes HS6 de la nomenclature 2007}
#'   \item{zero}{a dummy indicating whether the elasticity from the estimation was non-significant at the 1% level}
#'   \item{positive}{a dummy indicating whether the elasticity from the estimation was positive and significant}
#'   \item{sigma}{the value of trade elasticity}
#' }
#' @source <http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=35>
#' @source [Fontagné L., Guimbard H. and Orefice G. (2022) Product-Level Trade Elasticities. Journal of International Economics, vol 137](https://www.sciencedirect.com/science/article/abs/pii/S0022199622000253?via%3Dihub)
"ptle"
