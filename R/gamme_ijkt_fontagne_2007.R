#' @title
#' Calcul de gamme selon la méthode de Fontagné, Gaulier & Zignago (2007)
#'
#' @description
#' Calcul des gammes selon la méthode de Fontagné, Gaulier & Zignago (2007).
#' Chaque flux est divisé en une ou deux lignes selon sa/ses gammes.
#' Avec cette méthode il n'est pas conseillé d'effectuer des calculs de gammes
#' sur un dossier parquet possédant d'autres calculs de gammes, le nombre de
#' lignes n'étant pas le même.
#'
#' @details
#' Les gammes sont définies de la façon suivante : un ratio r est calculé pour
#' chaque flux s de la façon suivante :
#' \eqn{r = UV_s/UV_m} avec \eqn{UV_s} la valeur unitaire du flux s et
#' \eqn{UV_{m}} la médiane pondérée  par les valeurs commerciales des valeurs
#' unitaires des flux du groupe (t, k).
#'
#' - Si \eqn{r < 1}, alors la part du flux s dans la gamme Low est
#' \eqn{1 - r^{\alpha}} et la part du flux s dans la gamme Medium est
#' \eqn{r^{\alpha}}.
#' - Si \eqn{r > 1}, alors la part du flux s dans la gamme High est
#' \eqn{1 - 1/r^{\alpha}} et la part du flux s dans la gamme Medium est
#' \eqn{1/r^{\alpha}}.
#' - Si \eqn{r = 1}, alors la part du flux s dans la gamme Medium est 1.
#'
#' \eqn{\alpha} est un paramètre exogène qui va faire varier la répartition
#' entre les gammes. Plus il est petit, plus la part du commerce dans la gamme
#' moyenne sera grande.
#'
#' Les flux élémentaires ne sont pas forcément classés dans une uniquement
#' gamme. Un flux peut donc correspondre à une ou deux lignes. Dans ce cas il
#' faut bien utiliser la variable v_alpha qui représente la valeur du flux
#' dans la gamme correspondante et non la variable v qui représente la valeur
#' totale du flux.
#'
#' Cette fonction utilise les fonctionnalité du package
#' [arrow](https://arrow.apache.org/docs/r/) pour ne pas charger les données
#' en mémoire. Cependant, la fonction doit mettre en mémoire la base pour
#' calculer les médianes pondérées avant de la repasser en format arrow.
#' Si la base sur laquelle les gammes sont calculées est trop imposante pour
#' l'ordinateur, le calcul peut prendre du temps /
#' être interrompu pour cause de manque de mémoire. Dans ce cas, il est
#' conseillé de réduire le nombre d'années et de ré-exécuter plusieurs
#' fois la fonction pour arriver au bon nombre d'années.
#'
#'
#' @param baci Peut être un  chemin d'accès vers le dossier contenant
#' les données de BACI au format parquet. Peut également être un dataframe ou
#' bien des données au format arrow (requête ou non) permettant ainsi de chaîner
#' les opérations entre elles. ce paramètre est obligatoire.
#' @param alpha Valeur de alpha pour le calcul des gammes (paramètre qui va
#' faire varier la répartition entre les gammes)
#' @param years Années à garder dans la base. Par défaut, toutes les années
#' sont gardées.
#' @param codes Codes à garder dans la base. Par défaut, tous les codes sont
#' gardés. Code doit être une chaîne de caractères.
#' @param return_output Un booléen qui permet de retourner le résultat de la
#' fonction. Par défaut, FALSE.
#' @param path_output Chemin vers le dossier où le résultat de la fonction doit
#' être stocké en format parquet par année. Par défaut, NULL.
#' @param remove Un booléen qui permet de supprimer tous les fichiers commençant
#' par t= dans le path_output s'il est non nul. Par défaut, FALSE.
#' Evite les confusions si plusieurs utilisations dans le même dossier.
#' @param return_pq Booléen pour indiquer si les données doivent être retournées
#' en format arrow si TRUE. Par défaut : FALSE.
#'
#' @source [L.Fontagné, G.Gaulier & S.Zignago (2007),”Specialisation across Varieties within Products and North-South Competition”,CEPII Working Paper, N°2007-06, May](http://www.cepii.fr/PDF_PUB/wp/2007/wp2007-06.pdf)
#' @return Un dataframe / dossier parquet contenant les données de la base
#' BACI avec les gammes calculées
#' \describe{
#'   \item{i}{Code iso numérique de l'importateur}
#'   \item{j}{Code iso numérique de l'exportateur}
#'   \item{k}{Code HS6 du produit (en chaîne de caractère)}
#'   \item{t}{Année}
#'   \item{v}{Valeur totale du flux en milliers de dollars courants}
#'   \item{q}{Quantité du flux en tonnes métriques}
#'   \item{exporter}{Code iso3 de l'exportateur}
#'   \item{importer}{Code iso3 de l'importateur}
#'   \item{uv}{Valeur unitaire du flux en milliers de dollars courants par
#'   tonne métrique}
#'   \item{alpha_...}{Valeur du \eqn{\alpha} utilisé pour le calcul
#'   des gammes}
#'   \item{med_ref_ijkt}{Médiane pondérée par les valeurs unitaires des flux
#'   pour chaque groupe (t, k)}
#'   \item{r}{Ratio \eqn{r = UV_s/UV_m}}
#'   \item{share_L_...}{Part du flux s dans la gamme Low. Si négatif,
#'   le flux n'est pas dans la gamme}
#'   \item{share_M_...}{Part du flux s dans la gamme Medium. Si négatif,
#'   le flux n'est pas dans la gamme}
#'   \item{share_H_...}{Part du flux s dans la gamme High. Si négatif,
#'   le flux n'est pas dans la gamme}
#'   \item{v_...}{Valeur du flux s dans la gamme correspondante en milliers
#'   de dollars courants}
#'   \item{gamme_fontagne_2007_...}{Gamme de valeur unitaire du flux s}
#' }
#' @export
#'
#' @examples # Pas d'exemples.
gamme_ijkt_fontagne_2007 <- function(baci, alpha = 3,
                                     years = NULL, codes = NULL,
                                     return_output = FALSE, path_output = NULL,
                                     remove = FALSE, return_pq = FALSE){

  # Définition des messages d'erreur ----------------------------------------
  # Message d'erreur si alpha n'est pas un numérique
  if(!is.numeric(alpha)){
    stop("alpha doit \uEAtre un num\uE9rique.")
  }

  # Message d'erreur si alpha n'est pas unique
  if(length(alpha) != 1){
    stop("alpha doit \uEAtre un unique num\uE9rique.")
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

  # Message d'erreur si path_output n'est pas NULL et n'est pas une chaine de caractère
  if(!is.null(path_output) & !is.character(path_output)){
    stop("path_output doit \uEAtre NULL ou un chemin d'acc\uE9s sous forme de cha\uEEne de caract\uE9res.")
  }

  # Message d'erreur si remove n'est pas un booléen
  if(!is.logical(remove)){
    stop("remove doit \uEAtre un bool\uE9en.")
  }


  # Si remove == TRUE alors supprimer tous les fichiers commençant par t= dans le path_output s'il est non nul
  if(remove == TRUE & !is.null(path_output)){
    # supprimer les dossier commençant par t= : les dossier parquet par année
    list.files(path_output, pattern = "t=", full.names = TRUE) |>
      unlink(recursive = TRUE)
  }

  # Message d'erreur si return_pq n'est pas un booléen
  if (!is.logical(return_pq)) {
    stop("return_pq doit \uEAtre un bool\uE9en")
  }

  # Message d'avertissement si return_output = FALSE et return_pq = TRUE
  if (return_output == FALSE & return_pq == TRUE){
    message("Les donn\uE9es ne seront pas retourn\uE8es car return_output = FALSE")
  }


  # Filtrage des données ----------------------------------------------------
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


  # Définition des gammes ---------------------------------------------------
  # Transforme alpha en un object arrow pour forcer la compéhension de la variable
  # Permet d'être sûr que la variable est bien comprise par les fonctions arrow
  seuil_alpha <-
    alpha |>
    arrow::arrow_array()

  df_baci <-
    df_baci |>
    # Ajouter la valeur de alpha dans une variable 'alpha_{valeur de alpha}'
    # Les noms des variables contiennent la valeur de alpha pour plus de compréhension lors d'une ouverture future de la base
    # Calcul de la valeur unitaire de chaque flux
    dplyr::mutate(
      !!paste0("alpha_", seuil_alpha) := seuil_alpha,
      uv = v / q
    ) |>
    # Mise en mémoire de la base pour le calcul des médianes pondérées (impossible autrement)
    dplyr::collect() |>
    # Calcul de la médiane pondérée par la valeur pour chaque groupe {t, k}
    dplyr::mutate(
      .by = c(t, k),
      med_ref_ijkt = matrixStats::weightedMedian(uv, w = v, na.rm = TRUE)
    ) |>
    # Repassage en format arrow pour la suite des calculs
    arrow::arrow_table() |>
    # calcul des gammes
    dplyr::mutate(
      # Calculer le ratio r = uv / med_ref_ijkt
      r = uv / med_ref_ijkt,
      # Créer trois variable indiquant la part de chaque flux dans chaque gamme
      # si la part est négative, cela signifie que le flux n'est pas dans la gamme
      # Les noms de variables inclus la valeur de alpha pour plus de compréhension lors d'une ouverture future de la base
      !!paste0("share_L_", seuil_alpha) :=
        dplyr::case_when(
          r < 1 ~ (1 - r ** !!dplyr::sym(paste0("alpha_", seuil_alpha))),
          .default = -1
        ),
      !!paste0("share_M_", seuil_alpha) :=
        dplyr::case_when(
          r < 1 ~ r ** !!dplyr::sym(paste0("alpha_", seuil_alpha)),
          r > 1 ~ (1 / r ** !!dplyr::sym(paste0("alpha_", seuil_alpha))),
          r == 1 ~ 1
        ),
      !!paste0("share_H_", seuil_alpha) :=
        dplyr::case_when(
          r > 1 ~ (1 - (1/r ** !!dplyr::sym(paste0("alpha_", seuil_alpha)))),
          .default = -1
        )
    )

  # Créer trois bases de données pour chaque gamme
  # Permet de pouvoir les fusionner et d'avoir la valeur de chaque flux dans chaque gamme par ligne
  df_gamme_L <-
    df_baci |>
    # Garder uniquement les flux qui ont une part dans la gamme Low
    dplyr::filter(!!dplyr::sym(paste0("share_L_",seuil_alpha)) != -1) |>
    # Calculer la valeur du flux dans la gamme Low
    dplyr::mutate(
      !!paste0("v_", seuil_alpha) := v * !!dplyr::sym(paste0("share_L_",seuil_alpha)),
      !!paste0("gamme_fontagne_2007_", seuil_alpha) := "L"
    )

  df_gamme_H <-
    df_baci |>
    # Garder uniquement les flux qui ont une part dans la gamme High
    dplyr::filter(!!dplyr::sym(paste0("share_H_",seuil_alpha)) != -1) |>
    # Calculer la valeur du flux dans la gamme High
    dplyr::mutate(
      !!paste0("v_", seuil_alpha) := v * !!dplyr::sym(paste0("share_H_",seuil_alpha)),
      !!paste0("gamme_fontagne_2007_", seuil_alpha) := "H"
    )

  df_gamme_M <-
    df_baci |>
    # Calculer la valeur du flux dans la gamme Medium : tous les flux ont une part dans la catégorie Medium
    dplyr::mutate(
      !!paste0("v_", seuil_alpha) := v * !!dplyr::sym(paste0("share_M_",seuil_alpha)),
      !!paste0("gamme_fontagne_2007_", seuil_alpha) := "M"
    )

  # Fusionner les trois bases de données pour obtenir la base finale
  # Toutes les colonnes sont les les mêmes
  # Comme aucun clée de fusion, fusionne sur toutes le svariables identiques
  # Revient à bind_rows() (arrow ne comprend pas cette fonction)
  df_baci <-
    df_gamme_L |>
    dplyr::full_join(df_gamme_M) |>
    dplyr::full_join(df_gamme_H)

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
