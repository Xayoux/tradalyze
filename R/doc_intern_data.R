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


# country_codes_V202401 ---------------------------------------------------
#' Codes pays utilisés dans la base BACI + correspondance avec codes iso3
#'
#' @format ## `country_codes_V202401`
#' Un dataframe avec 238 lignes et 2 colonnes :
#' \describe{
#'   \item{country_code}{Codes pays utilisés dans la base BACI}
#'   \item{country_iso3}{Correspondance avec les codes iso3}
#' }
#' @source <http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V202401.zip>
"country_codes_V202401"


# product_codes_HS92_V202401 ----------------------------------------------
#' Codes HS6 de la nomenclature 1992 utilisés dans la base de donnée BACI sur la période 1995-2022
#'
#' @format ## `product_codes_HS92_V202401`
#' Un dataframe avec 5022 lignes et 2 colonnes :
#' \describe{
#'   \item{code}{Codes HS6 de la nomenclature 1992}
#'   \item{description}{Description du produit}
#' }
#' @source <http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V202401.zip>
"product_codes_HS92_V202401"


# product_codes_HS96_V202401 ----------------------------------------------
#' Codes HS6 de la nomenclature 1996 utilisés dans la base de donnée BACI sur la période 1996-2022
#'
#' @format ## `product_codes_HS96_V202401`
#' Un dataframe avec 5115 lignes et 2 colonnes :
#' \describe{
#'   \item{code}{Codes HS6 de la nomenclature 1996}
#'   \item{description}{Description du produit}
#' }
#' @source <http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS96_V202401.zip>
"product_codes_HS96_V202401"


# product_codes_HS02_V202401 ----------------------------------------------
#' Codes HS6 de la nomenclature 2002 utilisés dans la base de donnée BACI sur la période 2002-2022
#'
#' @format ## `product_codes_HS92_V202401`
#' Un dataframe avec 5223 lignes et 2 colonnes :
#' \describe{
#'   \item{code}{Codes HS6 de la nomenclature 2002}
#'   \item{description}{Description du produit}
#' }
#' @source <http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS02_V202401.zip>
"product_codes_HS02_V202401"


# product_codes_HS07_V202401 ----------------------------------------------
#' Codes HS6 de la nomenclature 2007 utilisés dans la base de donnée BACI sur la période 2007-2022
#'
#' @format ## `product_codes_HS07_V202401`
#' Un dataframe avec 5050 lignes et 2 colonnes :
#' \describe{
#'   \item{code}{Codes HS6 de la nomenclature 2007}
#'   \item{description}{Description du produit}
#' }
#' @source <http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS07_V202401.zip>
"product_codes_HS07_V202401"


# product_codes_HS12_V202401 ----------------------------------------------
#' Codes HS6 de la nomenclature 2012 utilisés dans la base de donnée BACI sur la période 2012-2022
#'
#' @format ## `product_codes_HS12_V202401`
#' Un dataframe avec 5202 lignes et 2 colonnes :
#' \describe{
#'   \item{code}{Codes HS6 de la nomenclature 2012}
#'   \item{description}{Description du produit}
#' }
#' @source <http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS12_V202401.zip>
"product_codes_HS12_V202401"


# product_codes_HS17_V202401 ----------------------------------------------
#' Codes HS6 de la nomenclature 2017 utilisés dans la base de donnée BACI sur la période 2017-2022
#'
#' @format ## `product_codes_HS17_V202401`
#' Un dataframe avec 5384 lignes et 2 colonnes :
#' \describe{
#'   \item{code}{Codes HS6 de la nomenclature 2017}
#'   \item{description}{Description du produit}
#' }
#' @source <http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS17_V202401.zip>
"product_codes_HS17_V202401"


# product_codes_HS22_V202401 ----------------------------------------------
#' Codes HS6 de la nomenclature 2022 utilisés dans la base de donnée BACI à partir de 2022.
#'
#' @format ## `product_codes_HS22_V202401`
#' Un dataframe avec 5609 lignes et 2 colonnes :
#' \describe{
#'   \item{code}{Codes HS6 de la nomenclature 2022}
#'   \item{description}{Description du produit}
#' }
#' @source <http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS22_V202401.zip>
"product_codes_HS22_V202401"


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
