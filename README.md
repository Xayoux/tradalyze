
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tradalyze

<!-- badges: start -->
<!-- badges: end -->

Le but de ce package est de fournir toutes les fonctions nécessaires à
l’analyse de la compétitivité des pays sur un ensemble de produits HS6
donné.

## Installation

Vous pouvez installer le package depuis
[GitHub](https://github.com/Xayoux/tradalyze.git) avec :

``` r
# install.packages("devtools")
devtools::install_github("Xayoux/tradalyze")
```

Un exemple d'utilisation est donné sur la page [use-tradalyze](https://github.com/Xayoux/use-tradalyze). 

## Prérequis

Pour utiliser ce package, vous aurez besoin de télécharger les données
de la base de donnée BACI (Base pour l’Analyse du Commerce
International) du
[CEPII](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)

Afin de pouvoir faire correspondre les codes HS de la nomenclature la
plus récente à une autre nomenclature, vous aurez également besoin de
télécharger le package
[concordance](https://github.com/insongkim/concordance.git) depuis
[GitHub](https://github.com/insongkim/concordance.git) avec :

``` r
# install.packages("devtools")
devtools::install_github("insongkim/concordance")
```
