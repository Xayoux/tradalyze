#' @title
#' Evaluer le commerce restant après élimination des outliers
#'
#' @description
#' Permet de déterminr la part du commerce restante après élmination des
#' outliers pour plusieurs seuils différents.
#'
#' @details
#' La fonction permet de déterminer la part du commerce restante après
#' élimination des outliers pour plusieurs seuils différents.
#'
#' La détermination des outliers se fait grâce à la fonction
#' \link{clean_uv_outliers}. Les valeurs et quantités
#' totales des parties de BACI sélectionnées sont calculées avant et après
#' élimination des outliers, ce qui permet de déterminer la part du commerce
#' aussi bien en valeur qu'en quantité restante après élimination des outliers.
#' Trois méthodes de détermination des outliers des outliers sont disponibles
#' et son documentées dans la fonction \link{clean_uv_outliers}.
#'
#' Ce processus est effectué pour chaque seuil souhaité. les résultats sont
#' retournés sous forme de data.frame et/ou d'un graphique représentant les
#' données totales du commerce mais également les données par chapitre de
#' codes HS6 (les deux premiers chiffres du code HS6).
#'
#' @param path_baci_parquet Un chemin vers un dossier parquet contenant les
#' données BACI.
#' @param years Les années à considérer pour l'analyse. Si NULL, toutes les
#' années sont considérées.
#' @param codes Les codes HS6 à considérer pour l'analyse. Si NULL, tous les
#' codes HS6 sont considérés.
#' @param method Méthode de détermination des outliers. Peut être 'classic',
#' 'fh13' ou 'h06'. Par défaut, 'classic'.
#' @param seuil_H_vector Un vecteur contenant les seuils "hauts" à considérer
#' pour l'élimination des outliers. Les valeurs doivent être comprises entre
#' 0 et 1 pour les méthodes "classic" et "fh13".
#' @param seuil_L_vector Un vecteur contenant les seuils "bas" à considérer pour
#' l'élimination des outliers. Les valeurs doivent être comprises entre 0 et 1.
#' @param graph Un booléen indiquant si un graphique doit être généré ou non.
#' @param path_df_output Un chemin vers un fichier csv où sauvegarder les
#' résultats de l'analyse. Si NULL, les résultats ne sont pas sauvegardés.
#' @param path_graph_output Un chemin vers un fichier où sauvegarder le
#' graphique généré. Si NULL, le graphique n'est pas sauvegardé.
#'
#' @return Un data.frame contenant les résultats de l'analyse et/ou un
#' graphique.
#' @export
#'
#' @examples # Pas d'exemple.
eval_outliers_share <- function(path_baci_parquet, years = NULL, codes = NULL,
                          method = "classic", seuil_H_vector, seuil_L_vector,
                          graph = TRUE, path_df_output = NULL,
                          path_graph_output = NULL){

  # Messages d'erreur -------------------------------------------------------

  # Message d'erreur si path_baci_parquet n'est pas une chaine de caractères
  if(!is.character(path_baci_parquet)){
    stop("path_baci_parquet doit \uEAtre une cha\uEEne de caract\uE8res.")
  }

  # Message d'erreur si years n'est pas NULL ou numérique
  if(!is.null(years) & !is.numeric(years)){
    stop("years doit \uEAtre NULL ou un vecteur num\uE9rique.")
  }

  # Message d'erreur si codes n'est pas NULL ou caractère
  if(!is.null(codes) & !is.character(codes)){
    stop("codes doit \uEAtre NULL ou un vecteur de caract\uE8res.")
  }

  # Message d'erreur si method n'est pas "classic", "fh13" ou "h06"
  if(!(method %in% c("classic", "fh13", "h06"))){
    stop("method doit \uEAtre 'classic', 'fh13' ou 'h06'.")
  }

  # Message d'erreur si seuil_H_vector n'est pas un vecteur numérique
  if(!is.numeric(seuil_H_vector)){
    stop("seuil_H_vector doit \uEAtre un vecteur num\uE9rique.")
  }

  # Message d'erreur si seuil_L_vector n'est pas un vecteur numérique
  if(!is.numeric(seuil_L_vector)){
    stop("seuil_L_vector doit \uEAtre un vecteur num\uE9rique.")
  }

  # Message d'erreur si seuil_H_vector et seuil_L_vector n'ont pas la même
  # longueur
  if(length(seuil_H_vector) != length(seuil_L_vector)){
    stop("seuil_H_vector et seuil_L_vector doivent avoir la m\uEAme longueur.")
  }

  # Message d'erreur si graph n'est pas un booléen
  if(!is.logical(graph)){
    stop("graph doit \uEAtre un bool\uE9en.")
  }

  # Message d'erreur si path_df_output n'est pas une chaine de caractères
  if(!is.null(path_df_output) & !is.character(path_df_output)){
    stop("path_df_output doit \uEAtre une cha\uEEne de caract\uE8res.")
  }

  # Message d'erreur si path_graph_output n'est pas une chaine de caractères
  if(!is.null(path_graph_output) & !is.character(path_graph_output)){
    stop("path_graph_output doit \uEAtre une cha\uEEne de caract\uE8res.")
  }


  # Création de la fonction principale --------------------------------------
  # Charger les données BACI en format parquet
  df_baci <-
    path_baci_parquet |>
    arrow::open_dataset()

  # Filtrer les données BACI si years n'est pas NULL
  if(!is.null(years)){
    df_baci <-
      df_baci |>
      dplyr::filter(t %in% years)
  }

  # Filtrer les données BACI si codes n'est pas NULL
  if(!is.null(codes)){
    df_baci <-
      df_baci |>
      dplyr::filter(k %in% codes)
  }

  # Calculer les valeurs et quantités totales par chapitre HS6
  df_commercial_value_chapter <-
    df_baci |>
    dplyr::mutate(
      chapter = substr(k, 1, 2)
    ) |>
    dplyr::summarize(
      .by = chapter,
      total_v = sum(v, na.rm = TRUE),
      total_q = sum(q, na.rm = TRUE)
    )

  # Calculer les valeurs et quantités totales pour tous les produits
  df_commercial_value_total <-
    df_baci |>
    dplyr::summarize(
      total_v = sum(v, na.rm = TRUE),
      total_q = sum(q, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      chapter = "total"
    )

  # Fusionner les totaux par chapitres et les totaux
  df_commercial_value <-
    df_commercial_value_chapter |>
    dplyr::collect() |>
    dplyr::bind_rows(df_commercial_value_total |> dplyr::collect())

  # Comparer les valeurs et quantités totales avec et sans outliers
  df_comparison <-
    # Appliquer la fonction de comparaison pour chaque seuil
    purrr::map2(
      seuil_H_vector,
      seuil_L_vector,
      \(seuil_H, seuil_L) comparison_outliers_func(
        df_commercial_value, seuil_H, seuil_L, method = method,
        path_baci_parquet = path_baci_parquet, years = years, codes = codes
      )
    ) |>
    # Regrouper les résultats en un seul data.frame
    purrr::list_rbind()

  # Créer un graphique si graph est TRUE
  if(graph == TRUE){
    graph <-
      df_comparison |>
      # Transformer les données en format long : insertion facile ds le graph
      tidyr::pivot_longer(
        cols = c(ratio_v, ratio_q),
        names_to = "ratio_type",
        values_to = "ratio_value"
      ) |>
      # Créer le graphique
      ggplot2::ggplot(ggplot2::aes(x = as.character(quantile_H), y = ratio_value, color = chapter))+
      ggplot2::geom_line(ggplot2::aes(group = chapter), linewidth = 1.1) +
      ggplot2::scale_color_brewer(palette = "Paired",) +
      ggplot2::labs(
        title = stringr::str_glue("Part du commerce total en enlevant les outliers avec la m\uE9thode {method}"),
        x = "Quantile Haut (%)",
        y = "Part du commerce total (%)",
        color = "Chapitres HS6"
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~ratio_type, scales = "free_y")
  } else {
    graph <- NULL
  }

  # Sauvegarder les résultats si path_df_output et/ou path_graph_output ne sont
  # pas NULL
  if(!is.null(path_df_output)){
    path_df_output |>
      readr::write_csv(df_comparison)
  }

  if(!is.null(path_graph_output)){
    path_graph_output |>
      ggplot2::ggsave(plot = graph, width = 15, height = 8)
  }

  # Retourner les résultats dans une liste
  return(list(df_comparison, graph))
}




# Fonction secondaire : Créer df de comparaison avec et sans outliers --------
# Pour un seul seuil
#'@noRd
comparison_outliers_func <- function(df_comm_value, seuil_H, seuil_L, method,
                                     path_baci_parquet, years, codes){

  # Calculer les valeurs et quantités totales sans outliers par chapitres HS6
  df_commercial_value_without_outliers_chapter <-
    # Déterminer les outliers et les éliminer
    clean_uv_outliers(
      path_baci_parquet = path_baci_parquet,
      method = method,
      years = years,
      codes = codes,
      seuil_H = seuil_H,
      seuil_L = seuil_L,
      visualisation = FALSE,
      return_output = TRUE
    ) |>
    arrow::arrow_table() |>
    # Calculer les valeurs et quantités totales par chapitre HS6
    dplyr::mutate(
      chapter = substr(k, 1, 2)
    ) |>
    dplyr::summarize(
      .by = chapter,
      total_v_without_outliers = sum(v, na.rm = TRUE),
      total_q_without_outliers = sum(q, na.rm = TRUE)
    )

  # Calculer les valeurs et quantités totales sans outliers pour tous les produits
  df_commercial_value_without_outliers_total <-
    df_commercial_value_without_outliers_chapter |>
    dplyr::summarize(
      total_v_without_outliers = sum(total_v_without_outliers, na.rm = TRUE),
      total_q_without_outliers = sum(total_q_without_outliers, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      chapter = "total"
    )

  # Fusionner les totaux par chapitres et les totaux
  df_commercial_value_without_outliers <-
    df_commercial_value_without_outliers_chapter |>
    dplyr::collect() |>
    dplyr::bind_rows(df_commercial_value_without_outliers_total |> dplyr::collect())

  df_commercial_value_comparison <-
    # Fusionner les valeurs et quantités totales avec et sans outliers
    df_comm_value |>
    dplyr::full_join(
      df_commercial_value_without_outliers,
      by = "chapter"
    ) |>
    # Calculer les ratios entre avec et sans outliers
    dplyr::mutate(
      ratio_v = total_v_without_outliers / total_v * 100,
      ratio_q = total_q_without_outliers / total_q * 100,
      quantile_H = seuil_H * 100,
      quantile_L = seuil_L * 100,
      method = {{method}}
    )

  return(df_commercial_value_comparison)
}
