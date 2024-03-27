#' @title
#' Calcul de gamme de valeurs unitaires selon la méthode de Fontagné, Freudenberg
#' et Péridy (1997)
#'
#' @description
#' Détermine la gamme à laquelle chaque flux appartient grâce à sa valeur
#' unitaire. Les trois gammes possibles sont Low (L), Medium (M) et High (H).
#' La méthode est basée sur la comparaison des valeurs unitaires avec la médiane
#' pondérée par la valeur commerciale des valeurs unitaires pour chaque marché
#' (produit-pays).
#'
#' @details
#' Les gammes sont déterminées de la façon suivante : la valeur unitaire de
#' chaque flux élémentaire est comparée à la médiane pondérée par la valeur
#' commerciale des valeurs unitaires pour chaque marché (produit-pays).
#' Si la valeur unitaire est supérieure à (1 + `alpha_H`) fois la médiane
#' pondérée, le flux est considéré comme High (H).
#' Si la valeur unitaire est inférieure à (1 / `alpha_L`) fois la médiane
#' pondérée, le flux est considéré comme Low (L). Sinon, le flux est
#' considéré comme Medium (M).
#'
#' Selon cette méthodologie, les seuils basiques sont `alpha_H` = 0.15
#' et `alpha_L` = 0.15 (Un écart de 15% à la médianne pondérée).
#'
#' Cette fonction permet de performer le calcul de gammes pour différents
#' seuils de manière simultanée.Chaque nom de variable dont la valeur varie
#' selon le seuil est renommée en incorporant la valeur du seuil pour permettre
#' de distinguer les différentes gammes.
#'
#' Cette dynamique des noms permet d'utiliser les données parquet créées pour
#' effectuer d'autres calculs de gammes (à l'exception des gammes de
#' fontagné et al 2007).
#'
#' Cette fonction utilise les fonctionnalité du package
#' [arrow](https://arrow.apache.org/docs/r/) pour performer des calculs sans
#' avoir à charger BACI en mémoire. Cependant le calcul de la médiane
#' pondérée nécessite le passage de la base (uniquement la partie nécessaire)
#' en mémoire. Si la base est trop importante, les calculs peuvent prendre un
#' certain temps, voir entraîner un problème de mémoire de l'ordinateur. Si
#' cela arrive, il est conseillé de réduire le nombre d'années sur lesquelles
#' la fonction doit calculer les gammes et d'exécuter plusieurs fois la fonction
#' jusqu'à avoir toutes les années voulues.
#'
#' @param path_baci_parquet Chemin vers le dossier où la base BACI est stockée
#' en format parquet.
#' @param alpha_H Seuil pour déterminer les gammes hautes. Par défaut, 0.15
#' (uv > 1.15 * medf_ref). Peut être un vecteur de numériques.
#' @param alpha_L Seuil pour déterminer les gammes basses. Par défaut, 0.15
#' (uv < 1/0.85 * medf_ref). Peut être un vecteur de numériques.
#' Doit avoir le même nombre de valeurs que alpha_H.
#' @param years Les années à considérer (un vecteur de numériques). Par défaut,
#' toutes les années sont prises en compte.
#' @param codes Les codes des produits à considérer (un vecteur de chaînes de
#' caractères). Par défaut, tous les produits sont pris en compte.
#' @param return_output Un booléen qui permet de retourner le résultat de la
#' fonction. Par défaut, la fonction ne retourne rien.
#' @param path_output Chemin vers le dossier où le résultat de la fonction doit
#' être stocké en format parquet par année. Par défaut, le résultat n'est
#' pas stocké.
#' @param remove Un booléen qui permet de supprimer tous les fichiers commençant
#' par t= dans le path_output s'il est non nul. Par défaut, FALSE.
#' Evite les confusions si plusieurs utilisations dans le même dossier.
#'
#' @return Un dataframe / dossier parquet contenant les données de la base BACI
#' avec une colonne supplémentaire indiquant la gamme des produits sur chaque
#' marché (produit-pays).
#' \describe{
#'   \item{i}{Code iso numérique de l'importateur}
#'   \item{j}{Code iso numérique de l'exportateur}
#'   \item{k}{Code HS6 du produit (en chaîne de caractère)}
#'   \item{t}{Année}
#'   \item{v}{Valeur totale du flux en milliers de dollars courants}
#'   \item{q}{Quantité du flux en tonnes métriques}
#'   \item{exporter}{Code iso3 de l'exportateur}
#'   \item{importer}{Code iso3 de l'importateur}
#'   \item{uv}{Valeur unitaire du flux en milliers de dollars courants/tonne
#'   métrique}
#'   \item{med_ref_t_k}{médiane pondérée par le commerce des valeurs unitaires
#'   des flux pour chaque groupe (t, k)}
#'   \item{gamme_fontagne_1997_...}{Gamme de la valeur unitaire du flux
#'   (H, M, L)}
#'   \item{seuil_H_...}{Seuil pour déterminer les gammes hautes}
#'   \item{seuil_L_...}{Seuil pour déterminer les gammes basses}
#' }
#' @export
#'
#' @examples # Pas d'exemples.
#' @source [Fontagné, L., Freudenberg, M., & Péridy, N. (1997). Trade patterns inside the single market (No. 97-07). Paris: CEPII.](http://cepii.fr/PDF_PUB/wp/1997/wp1997-07.pdf)
gamme_ijkt_fontagne_1997 <- function(path_baci_parquet, alpha_H = 0.15,
                                     alpha_L = alpha_H,
                                     years = NULL, codes = NULL,
                                     return_output = FALSE, path_output = NULL,
                                     remove = FALSE){


  # Définition des messages d'erreur ----------------------------------------

  # Message d'erreur si path_baci_parquet n'est pas une chaine de caractère
  if(!is.character(path_baci_parquet)){
    stop("path_baci_parquet doit être un chemin d'accès sous forme de chîne de caractères.")
  }

  # Message d'erreur si alpha_H ou alpha_L ne sont pas des numériques
  if(!is.numeric(alpha_H) | !is.numeric(alpha_L)){
    stop("alpha_H et alpha_L doivent être des numériques.")
  }

  # Message d'erreur si alpha_L n'a pas le même nombre de valeurs que alpha_H
  if(length(alpha_L) != length(alpha_H)){
    stop("alpha_L doit avoir le même nombre de valeurs que alpha_H.")
  }

  # Message d'erreur si years n'est pas NULL et n'est pas un vecteur de numériques
  if(!is.null(years) & !is.numeric(years)){
    stop("years doit être NULL ou un vecteur de numériques.")
  }

  # Message d'erreur si codes n'est pas NULL et n'est pas un vecteur de chaînes de caractères
  if(!is.null(codes) & !is.character(codes)){
    stop("codes doit être NULL ou un vecteur de chaînes de caractères.")
  }

  # Message d'erreur si return_output n'est pas un booléen
  if(!is.logical(return_output)){
    stop("return_output doit être un booléen.")
  }

  # Message d'erreur si path_output n'est pas NULL et n'est pas une chaine de caractère
  if(!is.null(path_output) & !is.character(path_output)){
    stop("path_output doit être NULL ou un chemin d'accès sous forme de chîne de caractères.")
  }

  # Message d'erreur si remove n'est pas un booléen
  if(!is.logical(remove)){
    stop("remove doit être un booléen.")
  }


  # Si remove == TRUE alors supprimer tous les fichiers commençant par t= dans le path_output s'il est non nul
  if(remove == TRUE & !is.null(path_output)){
    # supprimer les dossier commençant par t= : les dossier parquet par année
    list.files(path_output, pattern = "t=", full.names = TRUE) |>
      unlink(recursive = TRUE)
  }


  # Filtrage des données ----------------------------------------------------
  # Charger les données -> pas en mémoire grâce au package 'arrow'
  df_baci <-
    path_baci_parquet |>
    arrow::open_dataset()

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

  # Calcul des gammes pour chaque seuil
  for (i in 1:(length(alpha_H))){
    # Définir le seuil permettant de déterminer les gammes hautes
    seuil_H <-
      (1 + alpha_H[i]) |>
      arrow::arrow_array() # Passage en format arrow pour être sur qu'arrow comprenne

    # Définir le seuil permettant de déterminer les gammes basses
    seuil_L <-
      (1 + alpha_L[i]) |>
      arrow::arrow_array() # Passage en format arrow pour être sur qu'arrow comprenne

    # Calcul des gammes pour un seuil
    df_baci <-
      df_baci |>
      # Pour l'esthétique du dataframe
      dplyr::arrange(t) |>
      dplyr::relocate(t) |>
      # Calculer les valeurs unitaires
      dplyr::mutate(
        uv = v / q,
        seuil_H = seuil_H, # Intégrer le seuil H dans le dataframe
        seuil_L = seuil_L # Intégrer le seuil L dans le dataframe
      ) |>
      # Collecter (passage en R format) pour permettre le calcul de la médiane pondérée
      dplyr::collect() |>
      # Calcul de la médiane pondérée des uv par la valeur pour chaque marché k,t
      dplyr::mutate(
        .by = c(t, k),
        med_ref_t_k = matrixStats::weightedMedian(uv, w = v, na.rm = TRUE)
      ) |>
      # Passage au format arrow
      arrow::arrow_table() |>
      # Définition des gammes
      dplyr::mutate(
        gamme_fontagne_1997 =
          dplyr::case_when(
            uv > (seuil_H) * med_ref_t_k ~ "H",
            uv < (1 / (seuil_L)) * med_ref_t_k ~ "L",
            uv > (1 / (seuil_L)) * med_ref_t_k &  uv < (1 + seuil_H) * med_ref_t_k ~ "M"
          )
      ) |>
      # Renommer les variables de seuil et de gamme en rajoutant la valeur du
      # seuil pour permettre de calculer plusieurs seuils en même temps
      dplyr::rename(
        !!paste0("seuil_H_", unique(seuil_H)) := seuil_H,
        !!paste0("seuil_L_", unique(seuil_L)) := seuil_L,
        !!dplyr::if_else(
          alpha_H[i] == alpha_L[i],
          paste0("gamme_fontagne_1997_", unique(seuil_H)),
          paste0("gamme_fontagne_1997_", unique(seuil_L), "_", unique(seuil_H))
        ) := gamme_fontagne_1997
      )
  }

  # Enregistrer la nouvelle base en format parquet par année si path_output != NULL
  if(!is.null(path_output)){
    df_baci |>
      dplyr::group_by(t) |>
      arrow::write_dataset(path_output, format = "parquet")
  }

  # Retourner le résultat si return_output == TRUE
  if(return_output == TRUE){
    df_baci <-
      df_baci |>
      dplyr::collect()

    return(df_baci)
  }
}

