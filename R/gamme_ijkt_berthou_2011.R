# Fonction pour calculer la moyenne pondérée ------------------------------
#' Calcul une moyenne géométrique pondérée.
#'
#' @param x Un vecteur de données numériques.
#' @param w Un vecteur de poids.
#' @param ... Arguments supplémentaires à passer à la fonction `prod`.
#' @source rogiersbart with his RTOOLZ package : https://rdrr.io/github/rogiersbart/rtoolz/man/weighted.geomean.html
#'
#' @return La moyenne géométrique pondérée.
#'
#' @examples # Pas d'exemple.
#' @noRd
weighted_geomean <- function(x, w, ...){
  return(prod(x^w, ...)^(1/sum(w, ...)))
}


#' @title
#' Calcul des gammes de valeurs unitaires selon la méthode Berthou & Emlinger
#' (2011).
#'
#' @description
#' Détermine la gamme à laquelle appartient chaque flux commercial. Deux gammes
#' sont possibles : Low (L) et High (H). La gamme est déterminée en fonction de
#' la valeur unitaire du flux commercial. La valeur unitaire du flux commercial
#' est comparée à la moyenne géométrique pondérée des valeurs unitaires des
#' flux commerciaux.
#'
#' @details
#' Les gammes sont déterminées de la façon suivante : une moyenne géométrique
#' est calculée pour chaque couple {t, k, j} (année, produit, importateur).
#' Cette valeur indique la valeur unitaire moyenne des flux commerciaux pour
#' sur un produit donné pour un importateur donné sur une année donnée. La valeur
#' unitaire d'un flux commercial est comparée à cette moyenne géométrique. Si la
#' valeur unitaire est supérieure à la moyenne géométrique, le flux est classé
#' dans la gamme High (H). Sinon, le flux est classé dans la gamme Low (L).
#'
#' Cette fonction utilise les fonctionnalité du package
#' [arrow](https://arrow.apache.org/docs/r/) pour performer des calculs sans
#' avoir à charger BACI en mémoire. Cependant le calcul de la médiane
#' pondérée nécessite le passage de la base (uniquement la partie nécessaire)
#' en mémoire. Si la base est trop importante, les calculs peuvent prendre un
#' certain temps, voir entraîner un problème de mémoire de l'ordinateur.
#' Si cela arrive, il est conseillé de réduire le nombre d'années sur lesquelles
#' la fonction doit calculer les gammes et d'exécuter plusieurs fois la fonction
#' jusqu'à avoir toutes les années voulues.
#'
#' Il est possible d'utiliser cette fonction sur un dossier parquet dans lequel
#' des calculs de gamme ont déjà été effectués avec d'autres méthodes. Cependant,
#' il est fortement déconseillé de le faire sur un dossier où les gammes de
#' prix ont été calculés à partir de la méthode de Fontagné et al. (2007), les
#' lignes de flux n'étant plus uniques.
#'
#'
#' @param baci Peut être un  chemin d'accès vers le dossier contenant
#' les données de BACI au format parquet. Peut également être un dataframe ou
#' bien des données au format arrow (requête ou non) permettant ainsi de chaîner
#' les opérations entre elles. ce paramètre est obligatoire.
#' @param years Les années à considérer (un vecteur de numériques). Par défaut,
#' toutes les années sont prises en compte.
#' @param codes Les codes des produits à considérer (un vecteur de chaînes
#' de caractères). Par défaut, tous les produits sont pris en compte.
#' @param return_output Un booléen qui permet de retourner le résultat de la
#' fonction. Par défaut, la fonction ne retourne pas de dataframe.
#' @param path_output Chemin vers le dossier où le résultat de la fonction doit
#' être stocké en format parquet par année. Par défaut, le résultat n'est pas
#' stocké.
#' @param remove Un booléen qui permet de supprimer tous les fichiers commençant
#' par t= dans le path_output s'il est non nul. Par défaut, FALSE.
#' Evite les confusions si plusieurs utilisations dans le même dossier.
#' @param return_pq Booléen pour indiquer si les données doivent être retournées
#' en format arrow si TRUE. Par défaut : FALSE.
#'
#' @return Un dataframe / dossier parquet contenant les données de la base BACI
#' avec le calcul des gammes. Les variables du dataframe sont les suivantes :
#' \describe{
#'  \item{i}{Code iso numérique de l'importateur}
#'  \item{j}{Code iso numérique de l'exportateur}
#'  \item{k}{Code HS6 du produit (en chaîne de caractère)}
#'  \item{t}{Année}
#'  \item{v}{Valeur totale du flux en milliers de dollars courants}
#'  \item{q}{Quantité du flux en tonne métrique}
#'  \item{exporter}{Code iso3 de l'exportateur}
#'  \item{importer}{Code iso3 de l'importateur}
#'  \item{uv}{Valeur unitaire du flux en milliers dollars courants par
#'  tonne métrique}
#'  \item{geom_mean_weighted}{Moyenne géométrique pondérée par les valeurs des
#'  valeurs unitaires des couples {t, k,j}}
#'  \item{gamme_berthou_2011}{Gamme de valeur unitaire du flux commercial.
#'  Peut être 'L' ou 'H'}
#'  }
#' @export
#'
#' @examples # Pas d'exemples.
#' @source [A . Berthou, C . Emlinger (2011), « Les mauvaises performances françaises à l’exportation: La compé titivité prix est - elle coupable ? », La Lettre du CEPII , n°313, Septembre.](http://www.cepii.fr/PDF_PUB/lettre/2011/let313.pdf)
gamme_ijkt_berthou_2011 <- function(baci, years = NULL,
                                    codes = NULL, return_output = FALSE,
                                    path_output = NULL, remove = FALSE,
                                    return_pq = FALSE){

  # Messages d'erreur -------------------------------------------------------
  # Message d'erreur si years n'est pas NULL et n'est pas un vecteur de numériques
  if(!is.null(years) & !is.numeric(years)){
    stop("years doit \uEAtre NULL ou un vecteur de num\uE9riques.")
  }

  # Message d'erreur si codes n'est pas NULL et n'est pas un vecteur de chaînes de caractères
  if(!is.null(codes) & !is.character(codes)){
    stop("codes doit \uEAtre NULL ou un vecteur de cha\uEEnes de caract\uE8res.")
  }

  # Message d'erreur si path_output n'est pas NULL et n'est pas une chaine de caractère
  if(!is.null(path_output) & !is.character(path_output)){
    stop("path_output doit \uEAtre NULL ou un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
  }

  # Message d'erreur si return_output n'est pas un booléen
  if(!is.logical(return_output)){
    stop("return_output doit \uEAtre un bool\uE9en.")
  }

  # Message d'erreur si remove n'est pas un booléen
  if(!is.logical(remove)){
    stop("remove doit \uEAtre un bool\uE9en.")
  }

  # Message d'erreur si return_pq n'est pas un booléen
  if (!is.logical(return_pq)) {
    stop("return_pq doit \uEAtre un bool\uE9en")
  }

  # Message d'avertissement si return_output = FALSE et return_pq = TRUE
  if (return_output == FALSE & return_pq == TRUE){
    message("Les donn\uE9es ne seront pas retourn\uE8es car return_output = FALSE")
  }


  # Si remove == TRUE alors supprimer tous les fichiers commençant par t= dans le path_output s'il est non nul
  if(remove == TRUE & !is.null(path_output)){
    # supprimer les dossier commençant par t= : les dossier parquet par année
    list.files(path_output, pattern = "t=", full.names = TRUE) |>
      unlink(recursive = TRUE)
  }

  # Calcul des gammes -------------------------------------------------------
  # Ouvrir les données de BACI
  if (is.character(baci) == TRUE){
    # Ouvrir les données depuis un dossier parquet
    df_baci <-
      baci |>
      arrow::open_dataset()
  }
  else if (is.data.frame(baci) == TRUE){
    # Ouvrir les données depuis un dataframe : passage en format arrow
    df_baci <-
      baci |>
      arrow::arrow_table()
  }
  else{
    # Ouvrir les données depuis format arrow : rien à faire
    df_baci <- baci
  }

  # Garder les années voulues si years != NULL
  if(!is.null(years)){
    df_baci <-
      df_baci |>
      dplyr::filter(t %in% years)
  }

  # Garder les codes voulus si codes != NULL
  if(!is.null(codes)){
    df_baci <-
      df_baci |>
      dplyr::filter(k %in% codes)
  }

  # Calcul des gammes de valeur unitaires
  df_baci <-
    df_baci |>
    # Esthétique du df
    dplyr::arrange(t) |>
    # Calcul des valeurs unitaires
    dplyr::mutate(
      uv = v / q
    ) |>
    # Passage en mémoire pour pouvoir calculer la moyenne géométrique pondérée
    dplyr::collect() |>
    # Calcul de la moyenne géométrique pondérée pour chaque année, produit, importateur
    dplyr::mutate(
      .by = c(t, k, j),
      # Calcul de la part que représente le flux pour pour un produit d'un importateur sur une année donnée
      # Permet d'éviter des valeurs infinies dans le calcul de la moyenne géométrique pondérée
      v_share = v / sum(v, na.rm = TRUE),
      # Calcul de la moyenne géométrique pondérée
      geom_mean_weighted = analyse.competitivite::weighted_geomean(uv, v_share, na.rm = TRUE)
    ) |>
    # Passage en format arrow
    arrow::arrow_table() |>
    # Calcul des gammes de prix
    dplyr::mutate(
      gamme_berthou_2011 =
        dplyr::case_when(
          uv > geom_mean_weighted ~ "H",
          uv <= geom_mean_weighted ~ "L"
        )
    ) |>
    dplyr::select(!v_share)

  # Enregistrer la nouvelle base en format parquet par année si path_output != NULL
  if(!is.null(path_output)){
    df_baci |>
      dplyr::group_by(t) |>
      arrow::write_dataset(path_output, format = "parquet")
  }

  # Retourner le résultat si return_output == TRUE
  if (return_output == TRUE){
    if (return_pq == TRUE){
      return(df_baci)
    } else{
      df_baci <-
        df_baci |>
        dplyr::collect()
      return(df_baci)
    }
  }
}





