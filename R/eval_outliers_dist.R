# Fonction principale -----------------------------------------------------
#' @title
#' Evaluer la distribution de la différence entre les valeurs unitaires et leur
#' moyenne.
#'
#' @description
#' Permet de représenter graphiquement la distribution de la différence des
#' valeurs unitaires avec leur moyenne (par produit ou par année-produit). Ces
#' graphiques permettent de déterminer la présence d'outliers dans les données.
#'
#' @details
#' Les valeurs unitaires calculéesà partir de BACI peuvent souffrir de la
#' présence d'outliers à cause d'un mauvais renseignement des quantités dans
#' les données source de COMTRADE. Pour aider à déterminer les outliers dans
#' ces valeurs unitaires, cette fonction permet de représenter graphiquement
#' la distribution de la différence des valeurs unitaires avec leur moyenne
#' (par produit ou par année-produit), ce qui permet d'aggréger toutes les
#' mesures pour tous les produits et années dans un même graphique.
#'
#' Trois méthodes de détermination des outliers sont disponibles 'classic',
#' 'fh13' et 'h06' et sont décrites en détail dans l'aide de la fonction
#' \link{clean_uv_outliers}.
#'
#' Cette fonction peut prendre comme argument des vecteurs de numériques afin
#' de permettre l'exploration sur plusieurs seuils à partir d'un seul appel de
#' la fonction. Il est possible grâce au paramètre `wrap` de représenter les
#' distributions pour tous les produits ou les distributions par chapitre
#' de produits.
#'
#' Deux types de graphiques sont disponibles grâce au paramètre `graph_type` :
#' une courbe de densité ou bien une boîte à moustache.
#'
#' La fonction peut retourner un document excel dans lequel chaque feuille
#' correspond à un seuil spécifique. Ou bien elle peut renvoyer un fichier .png
#' par seuil.
#'
#' @param baci Peut être un  chemin d'accès vers le dossier contenant
#' les données de BACI au format parquet. Peut également être un dataframe ou
#' bien des données au format arrow (requête ou non) permettant ainsi de chaîner
#' les opérations entre elles. ce paramètre est obligatoire.
#' @param years Années à garder dans les données. Si NULL, toutes les années
#' sont gardées.
#' @param codes Codes à garder dans les données. Si NULL, tous les codes sont
#' gardés.
#' @param method Méthode de détermination des outliers. Peut être 'classic',
#' 'fh13' ou 'h06'.
#' @param seuil_H_vector Un vecteur contenant les seuils "hauts" à considérer
#' pour l'élimination des outliers. Les valeurs doivent être comprises entre
#' 0 et 1 pour les méthodes "classic" et "fh13".
#' @param seuil_L_vector Un vecteur contenant les seuils "bas" à considérer pour
#' l'élimination des outliers. Les valeurs doivent être comprises entre 0 et 1
#' pour les méthodes "classic" et "fh13".
#' @param graph_type Type de graphique voulu. Peut être "density" ou "boxplot"
#' @param ref Le reférentiel pour calculer la différence à la moyenne. Peut
#' être "k" pour calculer la différence par rapport à la moyenne du produit
#' toutes années confondues. Peut être "kt" pour calculer la différence par
#' rapport à la moyenne du produit pour chaque année.
#' @param wrap Booléen indiquant si le graphique doit représenter la distribution
#' globale (FALSE) ou la distribution par chapitre HS6 (FALSE).
#' @param output_type Le type de format de sortie voulu. Soit "xlsx", soit "png"
#' @param path_output Chemin du fichier excel ou seront enregistrées les données
#' si `output_type` = TRUE. Chemin vers le dossier où les fichiers png seront
#' enregistrés si `path_output` = "png".
#' @param print Un booléen indiquant si les graphiques doivent être affichés.
#' Dans le cas où le document de sortie est un fichier xlsx, alors les graphiques
#' sont forcément affichés.
#'
#' @return Un fichier excel ou des fichiers png.
#' @export
#'
#' @examples # Pas d'exemples.
eval_outliers_dist <- function(baci, years = NULL, codes = NULL,
                               method = "classic", seuil_H_vector, seuil_L_vector,
                               graph_type = "density", ref = "k",
                               wrap = TRUE, print = TRUE,
                               output_type = "xlsx", path_output = NULL) {

  # Liste contenant les graphiques pour chaque seuil
  list_graph <-
    purrr::map2(
      seuil_H_vector,
      seuil_L_vector,
      \(seuil_H, seuil_L) graph_outlier_dist(
        baci = baci,
        years = years,
        codes = codes,
        method = method,
        seuil_H = seuil_H,
        seuil_L = seuil_L,
        ref = ref,
        graph_type = graph_type,
        wrap = wrap
      )
    )

  # Créer le ficheir excel si output_type = "xlsx"
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
  }
  # Sinon créer les fichiers png
  else if (!is.null(path_output) & output_type == "png"){
    purrr::pmap(
      list(seuil_H_vector,
           seuil_L_vector,
           list_graph),
      \(seuil_H, seuil_L, graph) ggplot2::ggsave(
        filename = stringr::str_glue("{path_output}/outliers_distribution_seuil_H_{seuil_H}_seuil_L_{seuil_L}.png"),
        plot = graph,
        width = 15,
        height = 8
      )
    )
  }

  # Afficher les graphiques si voulus
  if(output_type == "png" & print == TRUE){
    purrr::map(
      list_graph,
      print
    )
  }
}



# Fonction secondaire -----------------------------------------------------
# Fonction pour enlever les outliers et créer le graphique correspondant au seuil
#'@noRd
graph_outlier_dist <- function(baci, years, codes,
                               method, seuil_H, seuil_L, ref, graph_type,
                               wrap){

  # Enlever les outliers selon la méthode et seuil voulu
  df_outliers <-
    clean_uv_outliers(
      baci = baci,
      years = years,
      codes = codes,
      method = method,
      seuil_H = seuil_H,
      seuil_L = seuil_L,
      visualisation = FALSE,
      path_output = NULL,
      return_output = TRUE,
      return_pq = FALSE
    ) |>
    # Calculer les valeurs unitaires et crer les chapitres HS6
    dplyr::mutate(
      uv = v / q,
      chapter = substr(k, 1, 2)
    )

  # Calculer la différence à la moyenne selon le référentiel produit
  if (ref == "k"){
    df_outliers <-
      df_outliers |>
      dplyr::mutate(
        .by = k,
        mean_diff = uv - mean(uv, na.rm = TRUE)
      )
  }
  # Calculer la différence à la moyenne selon le référentiel produit-année
  else if (ref == "kt"){
    df_outliers <-
      df_outliers |>
      dplyr::mutate(
        .by = c(k,t),
        mean_diff = uv - mean(uv, na.rm = TRUE)
      )
  }

  # Créer le graphique
  graph_dist <-
    df_outliers |>
    ggplot2::ggplot(ggplot2::aes(x = mean_diff))

  # Créer le graphique de densité
  if (graph_type == "density"){
    graph_dist <-
      graph_dist +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                              fill = "#D9D9D9", color = "#7F7F7F", na.rm = TRUE) +
      ggplot2::geom_density(na.rm = TRUE)
  }
  # Créer le boxplot
  else if (graph_type == "boxplot"){
    graph_dist <-
      graph_dist +
      ggplot2::geom_boxplot(na.rm = TRUE)
  }

  # Ajouter des éléments esthétiques au graphique
  graph_dist <-
    graph_dist +
    ggplot2::labs(
      title = "R\uE9partition des diff\uE9rences de valeurs unitaires par rapport \uE0 la moyenne",
      subtitle = stringr::str_glue("m\uE9thode : {method} ; seuil haut : {seuil_H} ; seuil bas : {seuil_L}"),
      x = stringr::str_glue("Diff\uE9rence entre la valeur unitaire et la moyenne (r\uE9f\uE9rentiel : {ref})")
    ) +
    ggplot2::scale_color_brewer(palette = "Paired") +
    ggplot2::theme_bw()

  # Wrap par chapitres
  if (wrap == TRUE){
    graph_dist <-
      graph_dist +
      ggplot2::facet_wrap(~chapter, scales = "free_x")
  }

  # Retourner le graphique
  return(graph_dist)
}



# Enregistrer graphs dans excel -------------------------------------------
# Fonction pour enregistrer un graphique dans un document excel
#' @noRd
save_graph_xlsx <- function(wb, path_excel_output, graph, seuil_H, seuil_L){

  # Ajouter une feuille avec comme nom les seuils choisis
  openxlsx::addWorksheet(wb, sheetName = stringr::str_glue("seuil_H-{seuil_H} seuil_L-{seuil_L}"))

  # Afficher le graph pour permettre son ajout dans le doc excel
  print(graph)

  # Insérer le graphique dans le document
  openxlsx::insertPlot(
    wb,
    sheet = stringr::str_glue("seuil_H-{seuil_H} seuil_L-{seuil_L}"),
    startRow = 1,
    startCol = 1,
    width = 15,
    height = 8
  )

  # Sauvegarder le document excel
  openxlsx::saveWorkbook(wb, path_excel_output, overwrite = TRUE)
}
