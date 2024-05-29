#' @title Cacule la qualité aggrégé pour un ensemble de produits
#'
#' @description
#' Calcule selon différentes manière une valeur aggrégée de la qualité pour
#' chaque produit/ensembles de produits.
#'
#' @details
#' Le calcul des données aggrégées de la qualité se fait obligatoirement
#' au niveau année-exporter. Il est possible de choisir sur quel ensemble de
#' produits, ces données sont calculées grâce au paramètre \code{var_aggregate}.
#'
#' 4 méthodes différentes sont disponibles pour effectuer l'aggrégations :
#' la moyenne arithmétique, la médiane, la moyenne arithmétique pondérée et la
#' médiane pondérée. Le choix de la variable de pondération s'effectue grâce au
#' paramètre \code{weighted_var}. Les pondérations peuvent varier chaque année,
#' ou bien être fixée à une année de référence grâce aux paramètres
#' \code{fixed_weight} et \code{year_ref}.
#'
#'
#' @param data_quality Les données contenant les informations sur la qualité
#' de chaque flux. Peut-être un dataframe, un fichier parquet ou un objet
#' arrow. Il est recommandé d'utilisé les données renvoyées par la fonction
#' \link{khandelwal_quality_eq}.
#' @param var_aggregate La variable servant à aggréger les données. Par
#' défaut, la variable \code{k} est utilisée.
#' @param method_aggregate La méthode d'aggrégation des données. Par défaut,
#' la médiane pondérée avec \code{weighted.median} est utilisée. Les autres
#' méthodes disponibles sont : \code{mean}, \code{median} et
#' \code{weighted.mean}.
#' @param weighted_var La variable servant à pondérer les données. Par défaut,
#' la variable \code{q} est utilisée. Utile uniquement si la méthode
#' utilisée correspond à \code{weighted.mean} ou \code{weighted.median}.
#' @param fixed_weight Si \code{TRUE}, les pondérations sont fixées à une
#' année de référence. Par défaut, \code{FALSE}.
#' @param year_ref L'année de référence pour les pondérations. Utile uniquement
#' si \code{fixed_weight} est \code{TRUE}.
#' @param print_output Si \code{TRUE}, affiche les données aggrégées. Par
#' défaut, \code{FALSE}.
#' @param return_output Si \code{TRUE}, renvoie les données aggrégées. Par
#' défaut, \code{TRUE}.
#' @param return_pq Si \code{TRUE}, renvoie les données de qualité sous format
#' arrow. Par défaut, \code{FALSE}.
#' @param path_output Le chemin où sauvegarder les données aggrégées. Par
#' défaut, \code{NULL}. Peut être un chemin vers un fichier parquet ou un
#' fichier csv ou xlsx.
#'
#' @return Un dataframe contenant les données aggrégées de la qualité.
#' @export
#'
#' @examples # Pas d'exemple
quality_aggregate <- function(data_quality, var_aggregate = "k",
                              method_aggregate = "weighted.median",
                              weighted_var = "q", fixed_weight = FALSE,
                              year_ref = NULL, print_output = FALSE,
                              return_output = TRUE,
                              return_pq = FALSE, path_output = NULL){

  # Ouvrir les données de data_quality
  if (is.character(data_quality) == TRUE){
    # Ouvrir les données depuis un dossier parquet
    df_data_quality <-
      data_quality |>
      arrow::open_dataset()
  }
  else if (is.data.frame(data_quality) == TRUE){
    # Ouvrir les données depuis un dataframe : passage au format arrow
    df_data_quality <-
      data_quality |>
      arrow::arrow_table()
  }
  else{
    # Ouvrir les données depuis format arrow : ne rien faire
    df_data_quality <-
      data_quality |>
      dplyr::collect()
  }

  # Agréger les données avec la moyenne
  if (method_aggregate == "mean"){
    df_quality_agg <-
      df_data_quality |>
      dplyr::summarize(
        .by = c(t, exporter, {{var_aggregate}}),
        quality = mean(quality, na.rm = TRUE)
      )
  }

  # Agréger les données avec la médiane
  if (method_aggregate == "median"){
    df_quality_agg <-
      df_data_quality |>
      dplyr::summarize(
        .by = c(t, exporter, {{var_aggregate}}),
        quality = median(quality, na.rm = TRUE)
      )
  }

  # Uniquement sur les calculs avec des pondérations
  # Si les poids sont fixes, calculés les poids de l'année de référence
  # Les associer aux flux
  if (fixed_weight == TRUE){
    df_pond <-
      df_data_quality |>
      dplyr::filter(t == year_ref) |>
      select(exporter, k, !!dplyr::sym(weighted_var))

    df_data_quality <-
      df_data_quality |>
      select(-c(!!dplyr::sym(weighted_var))) |>
      dplyr::left_join(
        df_pond,
        join_by(exporter, k)
      )
  }


  # Agréger les données avec la moyenne pondérée
  if (method_aggregate == "weighted.mean"){
    df_quality_agg <-
      df_data_quality |>
      dplyr::collect() |>
      dplyr::summarize(
        .by = c(t, exporter, {{var_aggregate}}),
        quality = stats::weighted.mean(quality, w = !!dplyr::sym(weighted_var), na.rm = TRUE)
      )
  }

  # Agréger les données avec la médiane pondérée
  if (method_aggregate == "weighted.median"){
    df_quality_agg <-
      df_data_quality |>
      dplyr::collect() |>
      dplyr::summarize(
        .by = c(t, exporter, {{var_aggregate}}),
        quality = matrixStats::weightedMedian(quality,
                                              w = !!dplyr::sym(weighted_var),
                                              na.rm = TRUE)
      )
  }


  if (print_output == TRUE){
    print(df_quality_agg |> dplyr::collect())
  }


  if (!is.null(path_output)){
    if (tools::file_ext(path_output) == "xlsx"){
      openxlsx::write.xlsx(
        df_quality_agg |> dplyr::collect(),
        path_output
      )
    }

    if (tools::file_ext(path_output) == "csv"){
      readr::write_csv(
        df_quality_agg |> dplyr::collect(),
        path_output
      )
    }

    if (tools::file_ext(path_output) == "parquet"){
      arrow::write_parquet(
        df_quality_agg |> arrow::arrow_table(),
        path_output
      )
    }
  }


  if (return_output == TRUE){
    if (return_pq == TRUE){
      df_quality_agg <-
        df_quality_agg |>
        arrow::arrow_table()

      return(df_quality_agg)
    }
    else{
      df_quality_agg <-
        df_quality_agg |>
        dplyr::collect()

      return(df_quality_agg)
    }
  }


}
