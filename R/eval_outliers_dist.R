# Fonction principale -----------------------------------------------------
#' Title
#'
#' @param path_baci_parquet
#' @param years
#' @param codes
#' @param method
#' @param seuil_H_vector
#' @param seuil_L_vector
#' @param graph_type
#' @param ref
#' @param wrap
#' @param output_type
#' @param path_output
#'
#' @return
#' @export
#'
#' @examples
eval_outliers_dist <- function(path_baci_parquet, years = NULL, codes = NULL,
                               method = "classic", seuil_H_vector, seuil_L_vector,
                               graph_type = "density", ref = "k",
                               wrap = TRUE,
                               output_type = "xlsx", path_output = NULL) {

  list_graph <-
    map2(
      seuil_H_vector,
      seuil_L_vector,
      \(seuil_H, seuil_L) graph_outlier_dist(
        path_baci_parquet = path_baci_parquet,
        years = years,
        codes = codes,
        method = method,
        seuil_H = seuil_H,
        seuil_L = seuil_L,
        ref = ref,
        graph_type = graph_type,
        wrap = TRUE
      )
    )

  if (!is.null(path_output) & output_type == "xlsx"){
    wb_outliers <- openxlsx::createWorkbook()

    purrr::pmap(
      list(seuil_H_vector,
           seuil_L_vector,
           list_graph),
      \(seuil_H, seuil_L, graph) save_graph_xlsx(
        wb = wb_outliers,
        path_excel_output = path_output,
        graph = graph,
        seuil_H = seuil_H,
        seuil_L = seuil_L
      )
    )
  } else if (!is.null(path_output) & output_type == "png"){
    purrr::pmap(
      list(seuil_H_vector,
           seuil_L_vector,
           list_graph),
      \(seuil_H, seuil_L, graph) ggsave(
        filename = str_glue("{path_output}/outliers_distribution_seuil_H_{seuil_H}_seuil_L_{seuil_L}.png"),
        plot = graph,
        width = 15,
        height = 8
      )
    )
  }
}



# Fonction secondaire -----------------------------------------------------
#'@noRd
graph_outlier_dist <- function(path_baci_parquet, years, codes,
                               method, seuil_H, seuil_L, ref, graph_type,
                               wrap){
  df_outliers <-
    clean_uv_outliers(
      path_baci_parquet = path_baci_parquet,
      years = years,
      codes = codes,
      method = method,
      seuil_H = seuil_H,
      seuil_L = seuil_L,
      visualisation = FALSE,
      path_output = NULL,
      return_output = TRUE
    ) |>
    mutate(
      uv = v / q,
      chapter = substr(k, 1, 2)
    )

  if (ref == "k"){
    df_outliers <-
      df_outliers |>
      mutate(
        .by = k,
        mean_diff = uv - mean(uv, na.rm = TRUE)
      )
  } else if (ref == "kt"){
    df_outliers <-
      df_outliers |>
      mutate(
        .by = c(k,t),
        mean_diff = uv - mean(uv, na.rm = TRUE)
      )
  }

  graph_dist <-
    df_outliers |>
    ggplot(aes(x = mean_diff))

  if (graph_type == "density"){
    graph_dist <-
      graph_dist +
      geom_density()
  } else if (graph_type == "boxplot"){
    graph_dist <-
      graph_dist +
      geom_boxplot()
  }

  graph_dist <-
    graph_dist +
    labs(
      title = "Répartition des différences de valeurs unitaires par rapport à la moyenne",
      subtitle = str_glue("m\uE9thode : {method} ; seuil haut : {seuil_H} ; seuil bas : {seuil_L}"),,
      x = "Différence entre la valeur unitaire et la moyenne (référentiel : {ref})"
    ) +
    scale_color_brewer(palette = "Paired") +
    theme_bw()

  if (wrap){
    graph_dist <-
      graph_dist +
      facet_wrap(~chapter, scales = "free_x")
  }

  return(graph_dist)
}



# Enregistrer graphs dans excel -------------------------------------------
#' @noRd
save_graph_xlsx <- function(wb, path_excel_output, graph, seuil_H, seuil_L){

  openxlsx::addWorksheet(wb, sheetName = str_glue("seuil_H-{seuil_H} seuil_L-{seuil_L}"))

  print(graph)

  openxlsx::insertPlot(
    wb,
    sheet = str_glue("seuil_H-{seuil_H} seuil_L-{seuil_L}"),
    startRow = 1,
    startCol = 1,
    width = 15,
    height = 8
  )

  openxlsx::saveWorkbook(wb, path_excel_output, overwrite = TRUE)
}
