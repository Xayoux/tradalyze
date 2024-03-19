
<!-- README.md is generated from README.Rmd. Please edit that file -->

# analyse.competitivite

<!-- badges: start -->
<!-- badges: end -->

Le but de ce package est de fournir toutes les fonctions nécessaires à
l’analyse de la compétitivité des pays sur un ensemble de produits HS6
donné.

## Installation

Vous pouvez installer le package depuis
[GitHub](https://github.com/Xayoux/analyse.competitivite.git) avec :

``` r
# install.packages("devtools")
devtools::install_github("Xayoux/analyse.competitivite")
```

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
- `create_baci_db` : Cette fonction permet de créer la base de donnée
  qui sera à la base de l’analyse. Cette base de donnée a pour source
  les données de
  [BACI](http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37).
  N’importe quelle nomenclature peut être choisie, il est cependant
  préférable de prendre la nomenclature de 1992. Cette fonction permet
  d’importer les données de la date de départ voulue à la date d’arrivée
  voulue, sans les stocker en mémoire et sans charger les années non
  voulues. Il est possible d’indiquer si l’on souhaite filtrer les
  produits, les exportateurs et/ou les importateurs. La fonction utilise
  le package [arrow](https://arrow.apache.org/docs/r/) ce qui permet de
  travailler sur les données sans ales charger en mémoire. Cependant, si
  le paramètre return = TRUE est activé, alors la base sera chargée en
  mémoire ce qui pourra entraîner un temps de traitement plus long.

## Explications détaillées des fonctions

### Extract product()

#### Arguments

La fonction `extract_product()` prend les arguments suivants :

- `codes_vector` : Un vecteur contenant les codes ou numéros de
  chapitres que l’on souhaite extraire d’une nomenclature HS6.

- `path_output` : Chemin d’accès au fichier de sortie. Doit être un
  fichier .xlsx ou .csv.

- `revision_origin` : Une chaîne de caractère indiquant la revision des
  codes HS d’origine (ceux dont on dispose et que l’on souhaite
  utiliser). Par defaut, `revision_origin = "HS22"`. Les valeurs
  possibles sont “HS22”, “HS92”, “HS96”, “HS02”, “HS07”, “HS12” et
  “HS17”.

- `revision_destination` : Une chaîne de caractère indiquant la revision
  des codes HS de destination (si l’on souhaite convertir nos codes HS
  pour convenir à notre nomenclature de BACI). Par defaut,
  `revision_destination = NULL` ce qui signifie qu’il n’y aura pas de
  conversion de codes. Les valeurs possibles sont “HS92”, “HS96”,
  “HS02”, “HS07”, “HS12” et “HS17” et NULL.

- `export` : Un booleen indiquant si l’on souhaite exporter le fichier.
  Par defaut, `export = TRUE`.

- `return_df` : Un booleen indiquant si l’on souhaite retourner le
  dataframe. Par defaut, `return_df = TRUE`.

- `correspondance` : Un booleen qui indique si l’on souhaite convertir
  les codes HS6 données en des codes HS6 d’une autre révision. Par
  defaut, correspondance = FALSE. Si `revision_destination != NULL`, ce
  paramètre doit être fixé à TRUE.

#### Fonctionnement

Le fonctionnement de cette fonction est le suivant :

A partir du paramètre `revision_origin`, la fonction va charger le
nomenclature de produits HS6 correspondante (disponible dans chaque
fichier BACI).

A partir de `codes_vector`, une expression régulière va être créée afin
de chercher tous les codes commençant par les valeurs indiquées dans le
paramètre :

``` r
# Exemple : je veux tous les codes du chapitre 51 et 52
codes_vector <- c(51, 52)

# La fonction va créer l'expression régulière suivante :
"^51|52"
```

Cette expression régulière est ensuite utilisée pour filtrer les codes
produits de la nomenclature en ne gardant que ceux qui correspondent aux
critères voulus. Si vous voulez tous les codes d’un chapitre sauf 1, il
faudra tous les écrire et non pas écrire le code du chapitre, sinon le
code non souhaité se retrouvera dans les résultats. La fonction va
également garder les descriptions des produits dans une autre colonne.

Si jamais vous indiquez vouloir faire une correspondance avec une autre
nomenclature **(si jamais une des nomenclature de la correspondance est
celle de 2022, soyez surs d’avoir téléchargé le package concordance via
github et non pas via CRAN sinon la fonction vous retournera une
erreur)**, la fonction va se servir de l’indication de la nomenclature
pour importer le fichier des codes produits HS6 correspondant. Elle va
ensuite convertir les paramètres `revision_origin` et
`revision_destination` pour qu’ils correspondent à ce qui est attendu
dans le package concordance.

La fonction `concord_hs()` est ensuite utilisée pour obtenir la
correspondance entre les deux nomenclatures.

Les descriptions des nouveaux codes obtenus sont ensuite ajoutés dans le
dataframe.

Enfin, les variables sont renommées de la façon suivante : les variables
de codes HS prennent le nom de `revision_origin` et
`revision_destination` tandis que les variables de description prennent
le nom de `description_revision_origin` et
`description_revision_destination`.

#### Output

La fonction retourne un dataframe et/ou enregistre un fichier xlsx ou
csv de 2 ou 4 variables selon si une correspondance entre deux
nomenclatures a été faite. Les variables sont les suivantes (les noms
correspondent aux valeurs prises par les paramètres `revision_origin` et
`revision_destination` :

- `revision_origin` : Codes HS6 qui ont été sélectionnés à partir des
  critères entrés en argument dans `codes_vector`.

- `description_revision_origin` : Description des produits HS6
  sélectionnés à partir des critères entrés en arguments

- `revision_destination` : Code HS6 obtenus à partir de la
  correspondance entre deux nomenclatures.

- `description_revision_destination` : Description des produits obtenus
  à partir de la correspondance entre deux nomenclatures.

### create_baci_db()

#### Arguments

- `folder_baci` : Un chemin d’accès vers le dossier contenant les
  fichiers BACI (ne pas modifier les noms des fichiers par rapport aux
  noms originaux).

<!-- -->

- `year_start` : Année de début de la base de données (Si NULL alors la
  date de départ sera l’année la plus ancienne des fichiers BACI
  disponibles dans le dossier).

- `year_end` : Année de fin de la base de données (Si NULL alors la date
  de fin sera l’année la plus récente des fichiers BACI).

- `hs_codes` : Un vecteur de chaîne de caractères indiquant les codes HS
  à garder (Si NULL alors tous les codes HS sont gardés).

- `exporter_codes` : Un vecteur de numériques indiquant les codes pays
  exportateurs à garder (Si NULL alors tous les pays exportateurs sont
  gardés).

- `importer_codes` : Un vecteur de numériques indiquant les codes pays
  importateurs à garder (Si NULL alors tous les pays importateurs sont
  gardés).

- `add_iso3` :Un booléen indiquant si les codes iso3 doivent remplacer
  les codes pays (TRUE par défaut).

- `calc_uv` : Un booléen indiquant si les valeurs unitaires doivent être
  calculées (TRUE par défaut).

- `path_output` : Un chemin d’accès vers le fichier csv de sortie (Si
  NULL alors aucun fichier n’est créé).

- `return_output` : Un booléen indiquant si le dataframe doit être
  retourné (FALSE par défaut). Attention si TRUE, le processus peut
  prendre très longtemps.

#### Fonctionnement

La fonction va commencer par définir une valeur pour `year_start` et/ou
`year_end` si aucune valeur n’est rentrée. Pour cela, elle regarde les
noms des fichiers BACI dans le dossier, les trie par ordre alphabétique
: si les noms n’ont pas été modifié comme cela a été indiqué, alors le
premier fichier sera celui concernant l’année la plus ancienne et le
dernier fichier sera celui concernant l’année la plus vieille. Il suffit
alors d’extraire les années des noms des fichiers BACI, de créer un
vecteur comprenant tous les nombres entiers entre les deux années, puis
de sélectionner uniquement les fichiers correspondant à ces années.

La fonction va ensuite importer les tous les fichiers sélectionnés, mais
grâce au package `arrow`, ils ne sont pas stockés en mémoire. Ils ne le
seront jamais au cours de l’exécution sauf si `return = TRUE`. Les
variables sont importées de telle sorte que `t, i, j` soient des
variables `integer` tandis que `k, v, q` sont importées en tant que
variable `string`. L’importation de `k` en tant que chaîne de caractère
permet de de ne pas perdre les 0 présents au début des codes s’il y en a
(comme conseillé dans la méthodologie d’utilisation de BACI). Les
variables `v` et `q` sont importées en tant que variables de chaîne de
caractère pour le cas où des espaces ou autres caractères auraient été
introduits/seraient compris par `arrow` comme étant des chaines de
caractères (le problème a été remarqué sur la variable `q` mais par
mesure de sécurité, la variable `v` a été soumise au même traitement).

Les données sont ensuite filtrées en fonction des critères indiqués. Les
codes HS doivent être les vrais codes HS6 et pas seulement des numéros
de chapitres.

Les variables `v` et `q` sont ensuite traitées pour enlever les
possibles espaces et être définies comme `NA` si jamais d’autres
caractère autres que des chiffres ou points s’y trouvent.

Si `add_iso3 = TRUE`, alors le dataframe va être fusionné pour les
variables `i` et `j` avec le dataframe de correspondance des codes pays
trouvable dans le package avec
`analyse.competitivite::country_codes_V202401`. Les variables `i` et `j`
deviennent non plus des codes pays, mais des codes iso3. La fonction va
ensuite sommer toutes les lignes pour lesquelels `i, j, k, t` sont les
mêmes (parfois deux codes pays partagent un même code iso, cette
procédure évite les doublons mêmes s’ils sont très très rares). cette
procédure va transformer les valeurs manquantes de `v` et `q` s’il y en
a en 0. Les 0 sont par la suite re-transformés en NA.

Si `calc_uv = TRUE` alors les valeurs unitaires de chaque couple
`i, j, k, t` sont calculées grâce au calcul suivant : `v / q`.

Le dataframe est ensuite exporté en csv et/ou retourné en tant que
dataframe R.

#### Output

Cette fonction va exporter un fichier csv e/ou renvoyer un dataframe R
contenant 6-7 variables :

- `i` : le code iso3 des pays exportateurs.

- `j` : le code iso3 des pays importateurs .

- `k` : le code HS6.

- `t` : l’année du flux.

- `v` la valeur du flux en milliers de dollars courants.

- `q` : la quantité du flux en tonnes métriques.

- `uv` : la valeur unitaire du flux en milliers de dollars courants par
  tonne métrique.
