# Documentation ---------------------------------------------------------------
#' @title Cacule la qualité aggrégé pour un ensemble de produits
#'
#' @description
#' Calcule selon différentes manière une valeur aggrégée de la qualité pour
#' chaque produit/ensembles de produits.
#'
#' @details
#' Le calcul des données aggrégées de la qualité se fait obligatoirement
#' au niveau année-exporter. Il est possible de choisir sur quel ensemble de
#' produits, ces données sont calculées grâce au paramètre \code{var_aggregate_k}.
#'
#' 4 méthodes différentes sont disponibles pour effectuer l'aggrégations :
#' la moyenne arithmétique, la médiane, la moyenne arithmétique pondérée et la
#' médiane pondérée. Le choix de la variable de pondération s'effectue grâce au
#' paramètre \code{weighted_var}. Les pondérations peuvent varier chaque année,
#' ou bien être fixée à une année de référence grâce aux paramètres
#' \code{fixed_weight} et \code{year_ref}.
#'
#' Pour utiliser correctement cette fonction, il faut que la fonction
#' \link{khandelwal_quality_eq} ait été utilisée au niveau des exportateurs et
#' non pas à un niveau plus agrégé de pays d'origine.
#'
#'
#' @param data_quality Les données contenant les informations sur la qualité
#' de chaque flux. Peut-être un dataframe, un fichier parquet ou un objet
#' arrow. Il est recommandé d'utilisé les données renvoyées par la fonction
#' \link{khandelwal_quality_eq}.
#' @param var_aggregate Vecteur contenant les variables (sous forme de chaînes
#' de caractères) à partir desquelle l'aggrégation doit se faire. Aucune
#' variable par défaut n'est mise. Il faut donc penser à bien toutes les
#' spécifier. 
#' @param method_aggregate La méthode d'aggrégation des données. Par défaut,
#' la médiane pondérée avec \code{weighted.median} est utilisée. Les autres
#' méthodes disponibles sont : \code{mean}, \code{median} et
#' \code{weighted.mean}.
#' @param weighted_var La variable servant à pondérer les données. Par défaut,
#' la variable \code{q} est utilisée. Utile uniquement si la méthode
#' utilisée correspond à \code{weighted.mean} ou \code{weighted.median}.
#' @param fixed_weight Si \code{TRUE}, les pondérations sont fixées à une
#' année de référence. Par défaut, \code{FALSE}.
#' @param var_desagregate Vecteur de variabvles (sous forme de chaînes de
#' caractères) indiquant le niveau le plus désagrégé possible des données
#' fournies pour l'année, les exportateurs, importateurs et les produits.
#' Ces variables servent à définir les poids fixes pour chaque flux. Par
#' défaut les variables sont `c("t", "exporter", "importer", "k")`. Il n'est
#' normalement pas nécessaire de les changer si les données utilisées
#' proviennent des fonctions de ce package. 
#' @param year_ref L'année de référence pour les pondérations. Utile uniquement
#' si \code{fixed_weight} est \code{TRUE}.
#' @param print_output Si \code{TRUE}, affiche les données aggrégées. Par
#' défaut, \code{FALSE}.
#' @param return_output Si \code{TRUE}, renvoie les données aggrégées. Par
#' défaut, \code{TRUE}.
#' @param base_100 Booléen indiquant si la qualité doit être calculée en base
#' 100 par rapport à une année de référence.
#' @param compare Booléen indiquant si la qualité en base 100 doit être comparée
#' à un pays de référence (par un ratio).
#' @param year_ref_base_100 L'année de référence à utiliser si la qualité doit
#' être calculée en base 100.
#' @param exporter_ref L'entité exportatrice de référence à utiliser pour
#' la comparaison avec un pays de référence. L'entité doit se trouver dans la
#' variable `var_aggregate_i`.
#' @param var_exporter_ref La variable contenant l'entité exportatrice qui
#' servira de référence pour la comparaison dans le cas où `compare = TRUE`.
#' @param return_pq Si \code{TRUE}, renvoie les données de qualité sous format
#' arrow. Par défaut, \code{FALSE}.
#' @param path_output Le chemin où sauvegarder les données aggrégées. Par
#' défaut, \code{NULL}. Peut être un chemin vers un fichier parquet ou un
#' fichier csv ou xlsx.
#' @return Un dataframe contenant les données aggrégées de la qualité.
#' @export
#'
#' @examples # Pas d'exemple
# Fonction quality_aggregate --------------------------------------------------
## Définition de la fonction --------------------------------------------------
quality_aggregate <- function(data_quality, var_aggregate,
                              method_aggregate = "weighted.median",
                              weighted_var = "q", fixed_weight = FALSE,
                              var_desagregate = c("t", "exporter", "importer", "k"),
                              year_ref = NULL, print_output = FALSE,
                              return_output = TRUE, base_100 = FALSE,
                              compare = FALSE, year_ref_base_100 = year_ref,
                              exporter_ref = NULL, var_exporter_ref = NULL,
                              return_pq = FALSE, path_output = NULL){

  ## Importer les données de qualité ----------------------------------------
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

  ## Calculer les qualités aggrégés selon les 4 méthodes --------------------
  # Agréger les données avec la moyenne
  if (method_aggregate == "mean"){
    df_quality_agg <-
      df_data_quality |>
      dplyr::summarize(
        .by = c({{var_aggregate}}),
        quality = mean(quality, na.rm = TRUE)
      )
  }

  # Agréger les données avec la médiane
  if (method_aggregate == "median"){
    df_quality_agg <-
      df_data_quality |>
      dplyr::summarize(
        .by = c({{var_aggregate}}),
        quality = stats::median(quality, na.rm = TRUE)
      )
  }

  # Uniquement sur les calculs avec des pondérations
  # Si les poids sont fixes, calculés les poids de l'année de référence
  # Les associer aux flux
  if (fixed_weight == TRUE){
    df_pond <-
      df_data_quality |>
      dplyr::filter(t == year_ref) |>
      dplyr::select({{var_desagregate}}, weighted_var) |>
      dplyr::select(-t)

    var_desagregate_join <-
      var_desagregate[var_desagregate != "t"]

    df_data_quality <-
      df_data_quality |>
      dplyr::select(-weighted_var) |>
      dplyr::left_join(
        df_pond,
        by = var_desagregate_join
      )
  }


  # Agréger les données avec la moyenne pondérée
  if (method_aggregate == "weighted.mean"){
    df_quality_agg <-
      df_data_quality |>
      dplyr::collect() |>
      dplyr::summarize(
        .by = c(var_aggregate),
        quality = stats::weighted.mean(quality, w = !!dplyr::sym(weighted_var), na.rm = TRUE)
      )
  }

  # Agréger les données avec la médiane pondérée
  if (method_aggregate == "weighted.median"){
    df_quality_agg <-
      df_data_quality |>
      dplyr::collect() |>
      dplyr::summarize(
        .by = c(var_aggregate),
        quality = matrixStats::weightedMedian(quality,
                                              w = !!dplyr::sym(weighted_var),
                                              na.rm = TRUE)
      )
  }

  ## ## Base 100 ------------------------------------------------------------
# IL FAUT EXCLURE LA VARIABLE t SI ELLE EST DANS LE VECTEUR DE var_aggregate !!
  
  # Passer les données en base 100 si voulu
  if (base_100 == TRUE){
    # Isoler la qualité pour l'année de référence
    # Sert de base pour la base 100
    df_quality_agg_year_ref <-
      df_quality_agg |>
      dplyr::filter(t == year_ref_base_100) |>
      # Enlever la variable t pour la fusion avec toutes les données
      dplyr::select(-t) |>
      dplyr::rename(quality_year_ref = quality)

    # Supprimer la variable t des variables servant à faire la fusion
    # Permet d'ajouter les valeurs pour les mêmes entités peu importe l'année
    var_aggregate_join <-
      var_aggregate[var_aggregate != "t"]
    
    # Calculer la qualité en base 100 en se basant sur l'année de référence
    df_quality_agg <-
      df_quality_agg |>
      dplyr::left_join(
        df_quality_agg_year_ref,
        by = c(var_aggregate_join)
      ) |>
      dplyr::mutate(
        quality_100 = quality / quality_year_ref * 100
      )
  }

  ## Comparaison avec un pays ---------------------------------------------------
  # Comparer les bases 100 des pays par rapport à un pays de référence
  if (compare == TRUE){

    var_aggregate_join_compare <-
      var_aggregate[var_aggregate != var_exporter_ref]
    
    # Isoler la qualité de l'exportateur de référence
    df_quality_agg_exporter_ref <-
      df_quality_agg  |>
      dplyr::filter(!!dplyr::sym(var_exporter_ref) == exporter_ref) |>
      dplyr::select({{var_aggregate_join_compare}}, t, quality_100) |>
      dplyr::rename(quality_100_exporter_ref = quality_100)

    # Calculer le ratio entre les qualités et la qualité du pays de référence
    df_quality_agg <-
      df_quality_agg |>
      dplyr::filter(!!dplyr::sym(var_exporter_ref) != exporter_ref) |>
      dplyr::left_join(
        df_quality_agg_exporter_ref,
        by = ({{var_aggregate_join_compare}})
      ) |>
      dplyr::mutate(
        quality_ratio = quality_100 / quality_100_exporter_ref
      )
  }

  ## Export des résultats -------------------------------------------------------
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
