#' Fonction qui permet de déterminer la gamme des produits sur chaque marché (produit-pays) en utilisant la méthode développée par Fontagné et al (1997)
#'
#' @param path_baci_parquet Chemin vers le dossier où la base BACI est stockée en format parquet.
#' @param alpha_H Seuil pour déterminer les gammes hautes. Par défaut, 0.15 (uv > 1.15 * medf_ref).
#' @param alpha_L Seuil pour déterminer les gammes basses. Par défaut, 0.15 (uv < 1/0.85 * medf_ref).
#' @param years Les années à considérer (un vecteur de numériques). Par défaut, toutes les années sont prises en compte.
#' @param codes Les codes des produits à considérer (un vecteur de chaînes de caractères). Par défaut, tous les produits sont pris en compte.
#' @param exporters Les pays exportateurs à considérer (un vecteur de chaînes de caractères ou de numériques). Par défaut, tous les pays sont pris en compte.
#' @param importers Les pays importateurs à considérer (un vecteur de chaînes de caractères ou de numériques). Par défaut, tous les pays sont pris en compte.
#' @param return_output Un booléen qui permet de retourner le résultat de la fonction. Par défaut, la fonction ne retourne rien.
#' @param path_output Chemin vers le dossier où le résultat de la fonction doit être stocké en format parquet par année. Par défaut, le résultat n'est pas stocké.
#' @param remove Un booléen qui permet de supprimer tous les fichiers commençant par t= dans le path_output s'il est non nul. Par défaut, FALSE. Evite les confusions si plusieurs utilisations dans le même dossier.
#'
#' @return Un dataframe / dossier parquet contenant les données de la base BACI avec une colonne supplémentaire indiquant la gamme des produits sur chaque marché (produit-pays).
#' @export
#'
#' @examples # Pas d'exemples?
#' @source Fontagné, L., Freudenberg, M., & Péridy, N. (1997). Trade patterns inside the single market (No. 97-07). Paris: CEPII.
gamme_ijkt_fontagne_1997 <- function(path_baci_parquet, alpha_H = 0.15,
                                     alpha_L = alpha_H,
                                     years = NULL, codes = NULL,
                                     exporters = NULL, importers = NULL,
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

  # Message d'erreur si exporters n'est pas NULL et n'est pas un vecteur de chaînes de caractères ou de numériques
  if(!is.null(exporters) & !is.character(exporters) & !is.numeric(exporters)){
    stop("exporters doit être NULL ou un vecteur de chaînes de caractères ou de numériques.")
  }

  # Message d'erreur si importers n'est pas NULL et n'est pas un vecteur de chaînes de caractères ou de numériques
  if(!is.null(importers) & !is.character(importers) & !is.numeric(importers)){
    stop("importers doit être NULL ou un vecteur de chaînes de caractères ou de numériques.")
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

  # Garder les exportateurs voulus si exporter != NULL
  # Filtrer par rapport aux codes iso numériques
  if(!is.null(exporters) & is.numeric(exporters)){
    df_baci <-
      df_baci |>
      dplyr::filter(i %in% exporters)
  }

  # Garder les exportateurs voulus si exporter != NULL
  # Filtrer par rapport aux codes iso3
  if(!is.null(exporters) & is.character(exporters)){
    df_baci <-
      df_baci |>
      dplyr::filter(exporter %in% exporters)
  }

  # Garder les importateurs voulus si importer != NULL
  # Filtrer par rapport aux codes iso numériques
  if(!is.null(importers) & is.numeric(importers)){
    df_baci <-
      df_baci |>
      dplyr::filter(j %in% importers)
  }

  # Garder les importateurs voulus si importer != NULL
  # Filtrer par rapport aux codes iso3
  if(!is.null(importers) & is.character(importers)){
    df_baci <-
      df_baci |>
      dplyr::filter(importer %in% importers)
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
        seuil_H = seuil_H,
        seuil_L = seuil_L
      ) |>
      # Collecter (passage en R format) pour permettre le calcul de la médianne pondérée
      dplyr::collect() |>
      # Calcul de la médianne pondérée des uv par la valeur pour chaque marché k,t
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
      # Renommer les variables de seuil et de gamme en rajoutant le seuil pour permettre de calculer plusieurs seuils en mêem temps
      rename(
        !!paste0("seuil_H_", unique(seuil_H)) := seuil_H,
        !!paste0("seuil_L_", unique(seuil_L)) := seuil_L,
        !!if_else(
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

