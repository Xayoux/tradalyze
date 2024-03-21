#' Fonction qui permet de déterminer la gamme des produits sur chaque marché (produit-pays) en utilisant la méthode développée par Fontagné et al (1997)
#'
#' @param path_baci_parquet Chemin vers le dossier où la base BACI est stockée en format parquet.
#' @param seuil Le seuil qui permet de déterminer les gammes (généralement 0.15 ou 0.25). Par défaut, le seuil est de 0.15.
#' @param years Les années à considérer (un vecteur de numériques). Par défaut, toutes les années sont prises en compte.
#' @param codes Les codes des produits à considérer (un vecteur de chaînes de caractères). Par défaut, tous les produits sont pris en compte.
#' @param exporters Les pays exportateurs à considérer (un vecteur de chaînes de caractères ou de numériques). Par défaut, tous les pays sont pris en compte.
#' @param importers Les pays importateurs à considérer (un vecteur de chaînes de caractères ou de numériques). Par défaut, tous les pays sont pris en compte.
#' @param return_output Un booléen qui permet de retourner le résultat de la fonction. Par défaut, la fonction ne retourne rien.
#' @param path_output Chemin vers le dossier où le résultat de la fonction doit être stocké en format parquet par année. Par défaut, le résultat n'est pas stocké.
#'
#' @return Un dataframe / dossier parquet contenant les données de la base BACI avec une colonne supplémentaire indiquant la gamme des produits sur chaque marché (produit-pays).
#' @export
#'
#' @examples # Pas d'exemples?
#' @source Fontagné, L., Freudenberg, M., & Péridy, N. (1997). Trade patterns inside the single market (No. 97-07). Paris: CEPII.
gamme_ijkt_fontagne_1997 <- function(path_baci_parquet, alpha = 0.15,
                                     years = NULL, codes = NULL,
                                     exporters = NULL, importers = NULL,
                                     return_output = FALSE, path_output = NULL){

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

  # Définir le seuil permettant de déterminer les gammes
  seuil <- 1 + alpha
  seuil <-
    seuil |>
    arrow::arrow_array() # Passage en format arrow pour être sur qu'arrow comprenne

  print(seuil)


  # Définition des gammes ---------------------------------------------------
  df_baci <-
    df_baci |>
    # Pour l'esthétique du dataframe
    dplyr::arrange(t) |>
    dplyr::relocate(t) |>
    # Calculer les valeurs unitaires
    dplyr::mutate(
      uv = v / q,
      seuil = seuil
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
          uv > (seuil) * med_ref_t_k ~ "H",
          uv < (1 / (seuil)) * med_ref_t_k ~ "L",
          uv > (1 / (seuil)) * med_ref_t_k &  uv < (1 + seuil) * med_ref_t_k ~ "M"
        )
    )

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

