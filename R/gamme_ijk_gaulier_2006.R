#' Fonction pour définir les gammes de prix selon la méthode de Gaulier et al (2006).
#'
#' @param path_baci_parquet Chemin vers le dossier où la base BACI est stockée en format parquet.
#' @param pond Un entier qui permet de définir la méthode de calcul des gammes de prix. Par défaut, pond = 1. Si pond = 1, les gammes sont calculées en utilisant les quantiles pondérés. Si pond = 2, les gammes sont calculées en utilisant les quantiles non pondérés. Si pond = 3, les gammes sont calculées en utilisant les quantiles pondérés et non pondérés.
#' @param years Les années à considérer (un vecteur de numériques). Par défaut, toutes les années sont prises en compte.
#' @param codes Les codes des produits à considérer (un vecteur de chaînes de caractères). Par défaut, tous les produits sont pris en compte.
#' @param exporters Les pays exportateurs à considérer (un vecteur de chaînes de caractères ou de numériques). Par défaut, tous les pays sont pris en compte.
#' @param importers Les pays importateurs à considérer (un vecteur de chaînes de caractères ou de numériques). Par défaut, tous les pays sont pris en compte.
#' @param return_output Un booléen qui permet de retourner le résultat de la fonction. Par défaut, la fonction ne retourne rien.
#' @param path_output Chemin vers le dossier où le résultat de la fonction doit être stocké en format parquet par année. Par défaut, le résultat n'est pas stocké.
#' @source G. Gaulier, F. Lemoine & D. Ünal-Kesenci (2006), “China's Emergence and the Reorganisation of Trade Flows in Asia”, CEPII Working Paper, n° 2006-05, March.
#'
#' @return Un dataframe / dossier parquet contenant les données de la base BACI avec une colonne supplémentaire indiquant la gamme des produits sur chaque marché (produit-pays).
#' @export
#'
#' @examples # Pas d'exemples.
gamme_ijkt_gaulier_2006 <- function(path_baci_parquet, pond = 1,
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

  df_baci <-
    df_baci |>
    dplyr::arrange(t) |>
    dplyr::mutate(
      uv = v / q
    ) |>
    dplyr::collect()

  if (pond == 1) {
    df_baci <-
      df_baci |>
      dplyr::mutate(
        .by = c(t, k),
        perc_33_pond = modi::weighted.quantile(uv, v, prob = 0.33),
        perc_67_pond = modi::weighted.quantile(uv, v, prob = 0.67)
      ) |>
      arrow::arrow_table() |>
      dplyr::mutate(
        gamme_gaulier_2006_pond =
          dplyr::case_when(
            uv <= perc_33_pond ~ "L",
            uv >= perc_67_pond ~ "H",
            uv > perc_33_pond & uv < perc_67_pond ~ "M"
          )
      )
  }
  else if (pond == 2){
    df_baci <-
      df_baci |>
      dplyr::mutate(
        .by = c(t, k),
        perc_33 = stats::quantile(uv, prob = 0.33, na.rm = TRUE),
        perc_67 = stats::quantile(uv, prob = 0.67, na.rm = TRUE)
      ) |>
      arrow::arrow_table() |>
      dplyr::mutate(
        gamme_gaulier_2006 =
          dplyr::case_when(
            uv <= perc_33 ~ "L",
            uv >= perc_67 ~ "H",
            uv > perc_33 & uv < perc_67 ~ "M"
          )
      )
  }
  else if (pond == 3){
    df_baci <-
      df_baci |>
      dplyr::mutate(
        .by = c(t, k),
        perc_33_pond = modi::weighted.quantile(uv, v, prob = 0.33),
        perc_67_pond = modi::weighted.quantile(uv, v, prob = 0.67),
        perc_33 = stats::quantile(uv, prob = 0.33, na.rm = TRUE),
        perc_67 = stats::quantile(uv, prob = 0.67, na.rm = TRUE)
      ) |>
      arrow::arrow_table() |>
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
  if(return_output == TRUE){
    df_baci <-
      df_baci |>
      dplyr::collect()

    return(df_baci)
  }
}
