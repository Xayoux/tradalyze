#' @title
#' Calcul des gammes de valeurs unitaires selon la méthode de Gaulier et al
#' (2006).
#'
#' @description
#' Cette fonction permet de calculer les gammes de valeurs unitaires des produits
#' en utilisant la méthode de Gaulier et al. (2006). Trois gammes sont définies :
#' Low (L), Medium (M) et High (H). La gamme est déterminée en fonction du
#' percentile de la valeur unitaire du flux commercial.
#'
#' @details
#' Les gammes sont déterminées de la façon suivante : Si la valeur unitaire du
#' flux commercial se trouve sous le 33e percentile des valeurs unitaires
#' (pondéré par le commerce ou non) pour les groupes (t, k), le flux est classé
#' dans la gamme Low (L). Si la valeur unitaire du flux commercial se trouve au
#' dessus du 67e percentile des valeurs unitaires (pondéré par le commerce ou non)
#' pour les groupes (t, k), le flux est classé dans la gamme High (H). Sinon, le
#' flux est classé dans la gamme Medium (M).
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
#' @param baci Peut être un  chemin d'accès vers le dossier contenant
#' les données de BACI au format parquet. Peut également être un dataframe ou
#' bien des données au format arrow (requête ou non) permettant ainsi de chaîner
#' les opérations entre elles. ce paramètre est obligatoire.
#' @param pond Un entier qui permet de définir la méthode de calcul des gammes
#' de prix. Par défaut, pond = 1. Si pond = 1, les gammes sont calculées en
#' utilisant les quantiles pondérés. Si pond = 2, les gammes sont calculées en
#' utilisant les quantiles non pondérés. Si pond = 3, les gammes sont calculées
#' en utilisant les quantiles pondérés et non pondérés (une variable pour
#' chaque).
#' @param years Les années à considérer (un vecteur de numériques). Par défaut,
#' toutes les années sont prises en compte.
#' @param codes Les codes des produits à considérer (un vecteur de chaînes de
#' caractères). Par défaut, tous les produits sont pris en compte.
#' @param return_output Un booléen qui permet de retourner le résultat de la
#' fonction. Par défaut, la fonction ne retourne rien.
#' @param path_output Chemin vers le dossier où le résultat de la fonction doit
#' être stocké en format parquet par année. Par défaut, le résultat n'est pas
#' stocké.
#' @param remove Un booléen qui permet de supprimer tous les fichiers commençant
#' par t= dans le path_output s'il est non nul. Par défaut, FALSE.
#' Evite les confusions si plusieurs utilisations dans le même dossier.
#' @param return_pq Booléen pour indiquer si les données doivent être retournées
#' en format arrow si TRUE. Par défaut : FALSE.
#'
#'
#' @source [G. Gaulier, F. Lemoine & D. Ünal-Kesenci (2006), “China's Emergence and the Reorganisation of Trade Flows in Asia”, CEPII Working Paper, n° 2006-05, March.](http://www.cepii.fr/PDF_PUB/wp/2006/wp2006-05.pdf)
#'
#' @return Un dataframe / dossier parquet contenant les données de la base BACI
#' et les gammes calculées. Les variables du dataframe sont les suivantes :
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
#'  \item{percentile_33}{33e percentile non pondéré des valeurs unitaires}
#'  \item{percentile_67}{67e percentile non pondéré des valeurs unitaires}
#'  \item{percentile_33_pond}{33e percentile pondéré par le commerce des
#'  valeurs unitaires}
#'  \item{percentile_67_pond}{67e percentile pondéré par le commerce des
#'  valeurs unitaires}
#'  \item{gamme_gaulier_2006_pond}{Gamme de valeur unitaire du flux commercial,
#'  selon les percentiles pondérés. Peut être 'L', 'M' ou 'H'}
#'  \item{gamme_gaulier_2006}{Gamme de valeur unitaire du flux commercial,
#'  selon les percentiles non pondérés. Peut être 'L', 'M' ou 'H'}
#'  }
#' @export
#'
#' @examples # Pas d'exemples.
gamme_ijkt_gaulier_2006 <- function(baci, pond = 1,
                                    years = NULL, codes = NULL,
                                    return_output = FALSE, return_pq = FALSE,
                                    path_output = NULL, remove = FALSE){


  # Messages d'erreur -------------------------------------------------------
  # Message d'erreur si pond n'est pas un entier compris entre 1 et 3
  if(!is.numeric(pond) | pond < 1 | pond > 3){
    stop("pond doit \uEAtre un entier compris entre 1 et 3.")
  }

  # Message d'erreur si years n'est pas NULL et n'est pas un vecteur de numériques
  if(!is.null(years) & !is.numeric(years)){
    stop("years doit \uEAtre NULL ou un vecteur de num\uE9riques.")
  }

  # Message d'erreur si codes n'est pas NULL et n'est pas un vecteur de chaînes de caractères
  if(!is.null(codes) & !is.character(codes)){
    stop("codes doit \uEAtre NULL ou un vecteur de cha\uEEnes de caract\uE8res.")
  }

  # Message d'erreur si return_output n'est pas un booléen
  if(!is.logical(return_output)){
    stop("return_output doit \uEAtre un bool\uE9en.")
  }

  # Message d'erreur si path_output n'est pas NULL et n'est pas une chaîne de caractère
  if(!is.null(path_output) & !is.character(path_output)){
    stop("path_output doit \uEAtre NULL ou un chemin d'acc\uE8s sous forme de cha\uEEne de caract\uE8res.")
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

  # Calcul des gammes
  df_baci <-
    df_baci |>
    # Pour l'esthétique du df
    dplyr::arrange(t) |>
    # Calcul de la valeur unitaire
    dplyr::mutate(
      uv = v / q
    ) |>
    # Passage en mémoire pour calculer les quantiles (pondérés)
    dplyr::collect()

  # Si pond = 1 : calcul des quantiles pondérés
  if (pond == 1) {
    df_baci <-
      df_baci |>
      # Pour chaque couple (t, k), calculer le 33e et 67 quantile pondéré
      dplyr::mutate(
        .by = c(t, k),
        perc_33_pond = modi::weighted.quantile(uv, v, prob = 0.33),
        perc_67_pond = modi::weighted.quantile(uv, v, prob = 0.67)
      ) |>
      # Passage au format arrow
      arrow::arrow_table() |>
      # Calcul de la gamme de Gaulier 2006 avec les quantiles pondérés
      dplyr::mutate(
        gamme_gaulier_2006_pond =
          dplyr::case_when(
            uv <= perc_33_pond ~ "L",
            uv >= perc_67_pond ~ "H",
            uv > perc_33_pond & uv < perc_67_pond ~ "M"
          )
      )
  }
  # Si pond = 2 : calcul des quantiles non pondérés
  else if (pond == 2){
    df_baci <-
      df_baci |>
      # Pour chaque couple (t, k), calculer le 33e et 67 quantile non pondéré
      dplyr::mutate(
        .by = c(t, k),
        perc_33 = stats::quantile(uv, prob = 0.33, na.rm = TRUE),
        perc_67 = stats::quantile(uv, prob = 0.67, na.rm = TRUE)
      ) |>
      # Passage au format arrow
      arrow::arrow_table() |>
      # Calcul de la gamme de Gaulier 2006 avec les quantiles non pondérés
      dplyr::mutate(
        gamme_gaulier_2006 =
          dplyr::case_when(
            uv <= perc_33 ~ "L",
            uv >= perc_67 ~ "H",
            uv > perc_33 & uv < perc_67 ~ "M"
          )
      )
  }
  # Si pond = 3 : calcul des quantiles pondérés et non pondérés
  else if (pond == 3){
    df_baci <-
      df_baci |>
      # Pour chaque couple (t, k), calculer le 33e et 67 quantile pondéré et non pondéré
      dplyr::mutate(
        .by = c(t, k),
        perc_33_pond = modi::weighted.quantile(uv, v, prob = 0.33),
        perc_67_pond = modi::weighted.quantile(uv, v, prob = 0.67),
        perc_33 = stats::quantile(uv, prob = 0.33, na.rm = TRUE),
        perc_67 = stats::quantile(uv, prob = 0.67, na.rm = TRUE)
      ) |>
      # Passage au format arrow
      arrow::arrow_table() |>
      # Calcul de la gamme de Gaulier 2006 avec les quantiles pondérés et non pondérés
      dplyr::mutate(
        gamme_gaulier_2006_pond =
          dplyr::case_when(
            uv <= perc_33_pond ~ "L",
            uv >= perc_67_pond ~ "H",
            uv > perc_33_pond & uv < perc_67_pond ~ "M"
          ),
        gamme_gaulier_2006 =
          dplyr::case_when(
            uv <= perc_33 ~ "L",
            uv >= perc_67 ~ "H",
            uv > perc_33 & uv < perc_67 ~ "M"
          )
      )
  }

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
    }
    else{
      df_baci <-
        df_baci |>
        dplyr::collect()

      return(df_baci)
    }
  }
}
