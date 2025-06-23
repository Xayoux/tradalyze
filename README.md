
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
devtools::install_github("Xayoux/tradalyze", ref = "refonte_totale")
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

## Fonctions

- `extract_product` : Cette fonction permet à partir de codes HS
  complets, des numéros de chapitres ou de section d’une nomenclature
  donnée, de créer la liste des codes HS utilisés, ainsi que d’afficher
  leur description. Il est possible d’établir la correspondance entre
  ces codes et ceux d’une autre nomenclature. Cette fonctionnalité
  utilise la fonction `concord_hs` du package
  [concordance](https://github.com/insongkim/concordance.git).

- `dl_baci` : Cette fonction permet de télécharger la base de données
  [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
  du [CEPII](http://www.cepii.fr/CEPII/en/welcome.asp) dans la
  nomenclature souhaitée (uniquement dernière version disponible), de
  dé-zipper le fichier .zip obtenu afin d’obtenir les fichier.csv de
  BACI (un fichier par année), ainsi que les fichiers csv correspondants
  aux codes produits et pays utilisés dans la base de données. Les
  fichiers .csv de BACI sont ensuite utilisés pour créer un dossier
  `BACI-parquet` qui contient la base BACI en format parquet disponible
  grâce au package [arrow](https://arrow.apache.org/docs/r/index.html).
  Les fichiers csv peuvent soit être gardés soit être supprimés pour
  gagner de la place. Toutes les fonctions nécessitant des données BACI
  utilisent les données en format parquet et non pas csv pour des
  questions de vitesse d’exécution et de mémoire. Il est donc conseillé
  d’utiliser cette fonction pour télécharger les données de BACI à
  l’endroit voulu (le téléchargement est optionnel si le fichier .zip
  est déjà présent dans le dossier).

- `transfo_baci_pq` : Cette fonction permet de transformer des fichiers
  .csv de BACI en format parquet. La fonction `dl_baci` dé-zip
  automatiquement le fichier .zip du dossier (qu’il vienne d’être
  téléchargé ou non). Si jamais vos fichiers.csv sont déjà dé-zippés,
  cette fonction permet de les convertir en format parquet directement
  et de les stocker dans le dossier `BACI-parquet`. Attention en
  utilisant cette fonction à ce que tous les dossiers de BACI soient
  bien présent pour éviter d’oublier une année. Il faut également
  vérifier que le fichier de codes pays soit bien disponible et n’ait
  pas été supprimé. Il est d’ailleurs conseillé de ne supprimer aucun
  fichier qui n’est pas supprimer par la fonction `dl_baci`.

- `gamme_ijkt_fontagne_1997` : Cette fonction permet de calculer les
  gammes de valeurs unitaires pour chaque flux commercial selon la
  méthode de
  [Fontagné-et-al-(1997)](http://cepii.fr/PDF_PUB/wp/1997/wp1997-07.pdf).
  Cette méthode permet de classer chaque flux dans une gammes : Low (L),
  Medium (M) et High (H) en fonction de la comparaison entre la valeur
  unitaire du flux et la médiane pondérée par le commerce des valeurs
  unitaires du produit. Cette fonction permet de calculer les gammes
  avec plusieurs seuils différents. Pour plus d’informations :
  `?gamme_ijkt_fontagne_1997`.

- `gamme_ijkt_gaulier_2006` : Cette fonction permet de calculer les
  gammes de valeur unitaire pour chaque flux commercial selon la méthode
  de
  [Gaulier-et-al-(2006)](http://www.cepii.fr/PDF_PUB/wp/2006/wp2006-05.pdf).
  Cette méthode permet de classer chaque flux dans une gammes : Low (L),
  Medium (M) et High (H) en fonction du percentile dans lequel se trouve
  la valeur unitaire du flux par rapport aux autres valeurs unitaires du
  produit considéré. Pour plus d’informations :
  `?gamme_ijkt_gaulier_2006`.

- `gamme_ijkt_fontagne_2007` : Cette fonction permet de calculer les
  gammes de valeur unitaire pour chaque flux commercial selon la méthode
  de
  [Fontagné-et-al-(2007)](http://www.cepii.fr/PDF_PUB/wp/2007/wp2007-06.pdf).
  Cette méthode permet de classer chaque flux dans au moins une des 3
  gammes : Low (L), Medium (M) et High (H). Un même flux peut être
  classé dans une ou deux gammes différentes (L-M ; M-H ; M). Chaque
  flux ne correspond donc plus à une ligne unique. Il y a une ligne par
  gamme pour chaque flux. A cause de cela il est très fortement
  déconseillé d’appliquer cette fonction à un dossier parquet ayant déjà
  subi des calculs de gamme, ou de performer des calcules de gamme sur
  un dossier parquet ayant subi un calcul de gamme selon cette méthode.
  Pour plus d’informations : `?gamme_ijkt_fontagne_2017`.

- `gamme_ijkt_berthou_2011` : Cette fonction permet de calculer les
  gammes de valeur unitaire pour chaque flux commercial selon la méthode
  de
  [Berthou-Emlinger-2011](http://www.cepii.fr/PDF_PUB/lettre/2011/let313.pdf).
  Cette méthode permet de classer chaque flux dans une gamme : Low (L)
  ou High (H) en fonction de la comparaison entre la valeur unitaire du
  flux et la moyenne géométrique pondérée par le commerce des valeurs
  unitaires du produit importé par un importateur donné. Pour plus
  d’informations : `?gamme_ijkt_berthou_2011`.

- `market_share` : Cette fonction permet de calculer les parts de marché
  par produits (ou à un niveau plus fin comme produit-importateur). Il
  est également possible de l’utiliser pour calculer les parts de marché
  à partir de zones géographiques agrégées : les paramètres de la
  fonction permettent de sélectionner les variables sur lesquelles
  calculer les parts de marché. Cependant les parts de marché sont
  obligatoirement calculées sur au moins la dimension temporelle,
  produit et une dimension régionale. Pour plus d’informations :
  `?market_share`.

- `clean_uv_outliers` : Cette fonction permet de déterminer (et enlever)
  les outliers dans les valeurs unitaires de BACI. Trois méthodes de
  détermination des outliers sont disponibles : une méthode “classic”
  qui enlève un certain pourcentage parmi les valeurs unitaires les plus
  basses et élevées pour chaque produit-année. Une méthode “fh13” qui
  provient de [Fontagné & Hatte
  (2013)](https://pse.hal.science/hal-00959394/) et une méthode “h06”
  qui provient de [Hallak
  (2006)](https://www.sciencedirect.com/science/article/abs/pii/S0022199605000516).
  Pour plus d’informations sur ces méthodes : `?clean_uv_outliers`

- `eval_outliers` : Cette fonction permet de fournir un dataframe ainsi
  qu’un graphique pour visualiser et analyser l’effet de différents
  seuils et méthodes dans le sélection des outliers sur la perte de
  valeur et de quantités dans les données de BACI. Cette fonction
  utilise la fonction `clean_uv_outliers`. Pour plus d’informations :
  `?eval_outliers`.

# Fonctions (provisoire):
- `add_chelem_classification` : ajoute la classification chelem aux données de BACI
- `adressed_deman` : calcule la demande adressée
- `clean_uv_outliers` : gérer les outliers
- `create_quality_df` : créer les données pour faire l'équation de khandelwal
- `dl_baci` : télécharger les données de BACI (pb ne fonctionne pas bien actuellement)
- `dl_gravity` : télécharger la base Gravity
- `eval_outliers_dist` : regarde la distribution des outliers
- `eval_outliers_share` : regarde ce que représente les outliers dans les données
- `extract_product` : extraire les codes produits souhaités
- `gamme_ijkt_gaulier_2006` : calcule les gamme selon la dite méthode
- `gamme_ijkt_berthou_2011` : calcule les gamme selon la dite méthode
- `gamme_ijkt_fontagne_1997` : calcule les gamme selon la dite méthode
- `gamme_ijkt_fontagne_2007` : calcule les gamme selon la dite méthode
- `graph_bar_comp_year` : graphiques barres de comparaison entre deux années
- `graph_lines_comparison` : graphique lignes
- `graph_market_share` : graph pour représenter parts de marchés
- `khandelwal_quality_eq` : estimer la régression de khandelwal
- `market_share` : calculer les parts de marché
- `quality_aggregate` : claculer la mesure agrégée de la qualité (après estimation de la régression)
- `uv_comp` : calculer et agréger les valeurs unitaires
