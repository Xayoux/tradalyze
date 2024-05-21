#' @title
#' Graphique de la demande adressée.
#'
#' @description
#' Cette fonction permet de générer un graphique représentant l'évolution de la
#' demande adressée en fonction du temps. Selon les variables indiquées en input,
#' la demande adressée peut être en valeur, en base 100 ou par rapport à un
#' pays de référence.
#'
#' @details
#' La fonction permet de créer un graphique pour représenter l'évolution de la
#' demande adressée en fonction du temps. Elle utilise les fonctions de ggplot2
#' pour la création du graphique.
#'
#' Il est important de noter, que cette fonction ne calcule pas la demande
#' adressée. Celle-ci doit être calculées précédemment (de préférence avec
#' la fonction \link{adressed_demand}). Il est possible de passer d'autres variables
#' que celles définies à partir des fonctions de ce package. Néanmoins cela reste
#' aux risques et périls de l'utilisateur si le rendu n'est pas cohérent.
#'
#' Grâce au paramètre `return_output`, il est possible de récupérer le graphique
#' afin de pouvoir le modifier presque entièrement par la suite.
#'
#' @param baci Les données de la demande adressée. Il est possible de passer
#' un chemin d'accès vers un fichier csv ou parquet ou un dataframe R.
#' @param x La variable à mettre en abscisse. Par défaut, c'est le temps : `t`
#' @param y La variable à mettre en ordonnée. Il n'y a pas de paramètre par
#' défaut, la variable choisie étant dépendante du type de demande adressée
#' calculée.
#' @param linewidth L'épaisseur de la ligne du graphique. Par défaut, c'est 1.
#' @param var_color La variable à mettre en couleur. Par défaut, c'est `NULL`.
#' @param palette_color La palette de couleur à utiliser. Par défaut, c'est `NULL`.
#' Aucune palette de couleur ne sera utilisée/
#' @param manual_color Les couleurs à utiliser. Par défaut, c'est `NULL`. Aucune
#' couleur ne sera utilisée.
#' @param na.rm Indique si les valeurs manquantes doivent être retirées. Par
#' défaut, c'est `TRUE`.
#' @param x_breaks Les breaks à utiliser sur l'axe des x. Par défaut, c'est `NULL`.
#' dans ce cas, les breaks vont du min au max avec un écart de 2 entre chaque valeur.
#' @param y_breaks Les breaks à utiliser sur l'axe des y. Par défaut, c'est `NULL`.
#' Dans ce cas, les valeurs sont prises par défaut.
#' @param x_title Le titre de l'axe des x. Par défaut, c'est `""`.
#' @param y_title Le titre de l'axe des y. Par défaut, c'est `""`.
#' @param title Le titre du graphique. Par défaut, c'est `""`.
#' @param subtitle Le sous-titre du graphique. Par défaut, c'est `""`.
#' @param caption La caption du graphique. Par défaut, c'est `""`.
#' @param color_legend Le titre de la légende des couleurs. Par défaut, c'est `""`.
#' @param type_theme Le type de thème à utiliser. Par défaut, c'est `bw`. Les
#' autres options sont `classic` et `minimal`.
#' @param var_facet La variable à utiliser pour les facettes. Par défaut, c'est
#' `NULL`. Si utilisé, le paramètres `scales = "free_y"` est utilisé.
#' @param path_output Le chemin d'accès pour sauvegarder le graphique. Par défaut,
#' c'est `NULL`.
#' @param width La largeur du graphique. Par défaut, c'est 15.
#' @param height La hauteur du graphique. Par défaut, c'est 8.
#' @param print Indique si le graphique doit être affiché. Par défaut, c'est `TRUE`.
#' @param return_output Indique si le graphique doit être retourné. Par défaut,
#' c'est `TRUE`.
#' @param var_linetype La variable qui va définir le type de ligne. Par défaut,
#' c'est `NULL`, les lignes seront toutes les mêmes.
#' @param manual_linetype Un vecteur indiquant à quelle entité quel type de ligne
#' doit correspondre.
#' @param linetype_legend Le titre de la légende des types de ligne. Par défaut,
#' c'est `""`.
#'
#' @return Un graphique représentant l'évolution de la demande adressée.
#' @export
#'
#' @examples # Pas d'exemples.
graph_adressed_demand <- function(baci, x = "t", y, linewidth = 1,
                                  var_linetype = NULL, manual_linetype = NULL,
                                  var_color = NULL, palette_color = NULL,
                                  manual_color = NULL,
                                  na.rm = TRUE, x_breaks = NULL, y_breaks = NULL,
                                  x_title = "", y_title = "", title = "",
                                  subtitle = "", caption = "", color_legend = "",
                                  linetype_legend ="", type_theme = "bw",
                                  var_facet = NULL, path_output = NULL,
                                  width = 15, height = 8, print = TRUE,
                                  return_output = TRUE){


  # Messages d'erreur -------------------------------------------------------

  # Ouvrir les données de baci depuis un chemin d'accès
  if (is.character(baci) == TRUE){
    # Ouvrir le ficheir csv
    if (tools::file_ext(baci) == "csv"){
      df_baci <- readr::read_csv(baci)
    }
    # Ouvrir fichier parquet
    else if (tools::file_ext(baci) == "pq"){
      df_baci <- arrow::read_parquet(baci)
    }
  }
  # Ouvrir baci depuis un dataframe R
  else if (is.data.frame(baci) == TRUE){
    df_baci <- baci
  }

  # Collecter les données (être sûr que les données sont en format R)
  df_baci <- dplyr::collect(df_baci)

  # si pas de x_breaks spécifié alors x va du min au max avec un écart de 2 entre chaque valeur
  if (is.null(x_breaks)){
    x_breaks <- seq(
      min(df_baci[x], na.rm = TRUE),
      max(df_baci[x], na.rm = TRUE),
      by = 2
    )
  }

  # Fondations du graph
  graph <-
    df_baci |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!dplyr::sym(x),
        y = !!dplyr::sym(y)
      )
    )

  # Définir le graphique
  graph <-
    graph +
    ggplot2::geom_line(
      na.rm = na.rm,
      linewidth = linewidth
    )

  # Définir les couleurs si souhaité
  if (!is.null(var_color)){
    graph <-
      graph +
      ggplot2::aes(
        color = !!dplyr::sym(var_color)
      )

    # Définir la palette de couleur si souhaitée
    if (!is.null(palette_color)){
      graph <-
        graph +
        ggplot2::scale_color_brewer(
          palette = palette_color
        )
    }

    # Définir les couleurs manuellement
    else if (!is.null(manual_color)){
      graph <-
        graph +
        ggplot2::scale_color_manual(
          values = manual_color
        )
    }
  }

  # Définir le type de ligne si souhaité
  if (!is.null(var_linetype)){
    graph <-
      graph +
      ggplot2::aes(
        linetype = !!dplyr::sym(var_linetype)
      )

    # Définir le type de ligne manuellement
    if (!is.null(manual_linetype)){
      graph <-
        graph +
        ggplot2::scale_linetype_manual(
          values = manual_linetype
        )
    }
  }

  # Fusionner les légendes de couleur et de type de ligne si les variables
  # sont les mêmes
  if (var_color == var_linetype){
    graph <-
      graph +
      ggplot2::guides(
        color = ggplot2::guide_legend(title = color_legend),
        linetype = ggplot2::guide_legend(title = linetype_legend)
      )
  }

  # Paramètres graphiques ---------------------------------------------------

  # Définir les breaks sur l'axe des x
  graph <-
    graph +
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    )

  # Définir les breaks sur l'axe des y si souhaité
  if (!is.null(y_breaks)){
    graph <-
      graph +
      ggplot2::scale_y_continuous(
        breaks = y_breaks
      )
  }

  # Titres et légendes
  graph <-
    graph +
    ggplot2::labs(
      x        = x_title,
      y        = y_title,
      title    = title,
      subtitle = subtitle,
      caption  = caption,
      color    = color_legend
    )

  # Thème général du graphique
  if (type_theme == "bw"){
    graph <-
      graph +
      ggplot2::theme_bw()
  }
  else if (type_theme == "classic"){
    graph <-
      graph +
      ggplot2::theme_classic()
  }
  else if (type_theme == "minimal"){
    graph <-
      graph +
      ggplot2::theme_minimal()
  }


  # Themes ------------------------------------------------------------------

  graph <-
    graph +
    ggplot2::theme(
      # Option des gridlines : les enlever
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),

      # Option des titres
      plot.title =
        ggplot2::element_text(
          size = 26,
          hjust = 0.5
        ),
      plot.subtitle =
        ggplot2::element_text(
          size = 22,
          hjust = 0.5
        ),
      plot.caption =
        ggplot2::element_text(
          size = 16,
          hjust = 0,
          color = "black"
        ),

      # Option du texte de l'axe des X
      axis.text.x =
        ggplot2::element_text(
          angle = 45,
          hjust = 1,
          size = 18,
          color = "black"
        ),
      axis.title.x =
        ggplot2::element_text(
          size = 22,
          vjust = -0.5
        ),

      # Option du texte de l'axe des Y
      axis.text.y =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      axis.title.y =
        ggplot2::element_text(
          size = 22
        ),

      # Options de la légende
      legend.position  = "right",
      legend.text =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      legend.key.spacing.y = ggplot2::unit(0.3, "cm"),
      legend.title =
        ggplot2::element_text(
          size = 22,
          color = "black",
          hjust = 0.5
        ),

      # Options des facettes
      strip.background =
        ggplot2::element_rect(
          colour = "black",
          fill = "#D9D9D9"
        ),
      strip.text =
        ggplot2::element_text(
          size = 18,
          color = "black"
        )
    )

  # Facettes
  if (!is.null(var_facet)){
    graph <-
      graph +
      ggplot2::facet_wrap(
        c(var_facet),
        scales = "free_y"
      )
  }


  # Exporter le graph -------------------------------------------------------
  if (print == TRUE){
    print(graph)
  }

  if (!is.null(path_output)){
    graph |>
      ggplot2::ggsave(
        filename = path_output,
        width = width,
        height = height
      )
  }

  if (return_output == TRUE){
    return(graph)
  }
}
