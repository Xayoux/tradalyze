#' @title
#' Graphique d'évolution des parts de marché
#'
#' @description
#' Fonction qui permet de créer un graphique pour représenter l'évolution des
#' parts de marché en fonction du temps. Le graphique peut être de type "area",
#' ou line avec ou sans point sur les observations.
#'
#' @details
#' La fonction permet de créer un graphique pour représenter l'évolution des
#' parts de marché en fonction du temps. Cette fonction utilise les fonctions
#' de ggplot2 pour la création du graphique.
#'
#' Il est important de noter, que cette fonction ne calcule pas les parts de
#' marché. Celles-ci doivent être calculées précédemment (de préférence avec
#' la fonction \link{market_share}). Il est possible de passer d'autres variables
#' que celles définies à partir des fonctions de ce package. Néanmoins cela reste
#' aux risques et périls de l'utilisateur si le rendu n'est pas cohérent.
#'
#' Grâce au paramètre `return_output`, il est possible de récupérer le graphique
#' afin de pouvoir le modifier presque entièrement par la suite.
#'
#'
#' @param baci Les données de BACI sous forme de chemin d'accès, de format
#' parquet ou de dataframe R.
#' @param x La variable correspondant à l'axe des abscisses. Par défaut et de
#' manière conseillé cette variable est "t.
#' @param y La variable correspondant à l'axe des ordonnées. Par défaut et de
#' manière conseillé cette variable est "market_share".
#' @param graph_type Le type de graphique à réaliser. Par défaut, le graphique
#' est de type "area". Les autres options sont "line" et "line_point". Pour
#' une représentation des parts de marché avec des lignes et potentiellement
#' des points sur les observations, il est conseillé de choisir area qui produit
#' un graphique en aire.
#' @param var_fill_color La variable catégorielle correspondant aux
#' différentes catégories à mettre en couleur.
#' Par défaut, cette variable est NULL.
#' @param palette_color La palette de couleur à utiliser pour le graphique.
#' Attention si le nombre de couleur dans la palette n'est pas suffisant par
#' rapport au nombre de catégories, une erreur sera retournée. Par défaut,
#' cette variable est NULL.
#' @param manual_color Les couleurs manuelles à utiliser pour le graphique.
#' Doit généralement être un vecteur contenant les couleurs utilisés et les
#' pays auxquels elles sont associées. Par défaut, cette variable est NULL.
#' @param percent Si TRUE, les valeurs de l'axe des ordonnées seront exprimées
#' en pourcentage. Par défaut, cette variable est TRUE. Attention, les valeurs
#' ne doit pas être exprimées de base de proportion, mais bien en pourcentage
#' (de 1 à 100).
#' @param na.rm Si TRUE, les valeurs manquantes seront retirées du graphique.
#' @param x_breaks Les valeurs à afficher sur l'axe des abscisses. Par défaut,
#' cette variable est NULL. Dans ce cas, les valeurs vont du minimum au maximum
#' de la variable x avec un écart de 2 entre chaque valeur.
#' @param x_title Le titre de l'axe des abscisses. Par défaut, cette variable
#' est vide.
#' @param y_title Le titre de l'axe des ordonnées. Par défaut, cette variable
#' est vide.
#' @param title Le titre du graphique. Par défaut, cette variable est vide.
#' @param subtitle Le sous-titre du graphique. Par défaut, cette variable est
#' vide.
#' @param caption La légende du graphique. Par défaut, cette variable est vide.
#' @param color_legend Le titre de la légende des couleurs. Par défaut, cette
#' variable est vide. Inutilisé si graph_type est "area".
#' @param fill_legend Le titre de la légende des couleurs de remplissage. Par
#' défaut, cette variable est vide. Inutilisé si graph_type est "line" ou
#' "line_point".
#' @param type_theme Le thème général du graphique. Par défaut, cette variable
#' est "bw". Les valeurs possibles sont "bw", "classic" et "minimal".
#' @param var_facet La variable à utiliser pour les facettes. Par défaut, cette
#' variable est NULL. Si NULL alors l'entièreté des données sera représentée
#' sur un seul graphique. Sinon, les données seront séparées en fonction des
#' différentes catégories présentes dans la variable (généralement une variable
#' de secteur).
#' @param path_output Le chemin d'accès pour sauvegarder le graphique. Par
#' défaut, cette variable est NULL. Si NULL, le graphique ne sera pas sauvegardé.
#' @param width La largeur du graphique. Par défaut, cette variable est 15.
#' @param height La hauteur du graphique. Par défaut, cette variable est 8.
#' @param print Si TRUE, le graphique sera affiché. Par défaut, cette variable
#' est TRUE.
#' @param return_output Si TRUE, le graphique sera retourné permettant sa
#' modification par l'utilisateur. Par défaut, cettevariable est TRUE.
#'
#' @return Un graphique représentant l'évolution des parts de marché en fonction
#' du temps.
#' @export
#'
#' @examples # Pas d'exemple
graph_market_share <- function(baci, x = "t", y = "market_share",
                               graph_type = "area",
                               var_fill_color = NULL, palette_color = NULL,
                               manual_color = NULL, percent = TRUE,
                               na.rm = TRUE, x_breaks = NULL,
                               x_title = "", y_title = "", title = "",
                               subtitle = "", caption = "", color_legend = "",
                               fill_legend = "", type_theme = "bw",
                               var_facet = NULL, path_output = NULL,
                               width = 15, height = 8, print = TRUE,
                               return_output = TRUE){


  # Messages d'erreur -------------------------------------------------------



  # Ouvrir les données de BACI
  # Ouvrir les données depuis un dossier parquet
  if (is.character(baci) == TRUE){
    df_baci <-
      baci |>
      arrow::open_dataset() |>
      dplyr::collect()
  }
  # Ouvrir les données depuis un dataframe R
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

  # Définir le type de graphique -----------------------------------------

  # Graph area
  if (graph_type == "area"){
    graph <-
      graph +
      ggplot2::geom_area(
        ggplot2::aes(
          fill = !!dplyr::sym(var_fill_color)
        ),
        na.rm = na.rm
      )

    # Couleurs du graph par palette
    if (!is.null(palette_color)){
      graph <-
        graph +
        ggplot2::scale_fill_brewer(palette = palette_color)
    }

    # Couleurs du graph manuelles
    if (!is.null(manual_color)){
      graph <-
        graph +
        ggplot2::scale_fill_manual(values = manual_color)
    }
  }
  # Graph line
  else if (graph_type == "line"){
    graph <-
      graph +
      ggplot2::geom_line(
        ggplot2::aes(
          color = !!dplyr::sym(var_fill_color)
        ),
        na.rm = na.rm
      )

    # Couleurs du graph par palette
    if (!is.null(palette_color)){
      graph <-
        graph +
        ggplot2::scale_color_brewer(palette = palette_color)
    }

    # Couleurs du graph manuelles
    if (!is.null(manual_color)){
      graph <-
        graph +
        ggplot2::scale_color_manual(values = manual_color)
    }
  }
  # Graph line_point (lignes avec des points sur les observations)
  else if (graph_type == "line_point"){
    graph <-
      graph +
      ggplot2::geom_line(
        ggplot2::aes(
          color = !!dplyr::sym(var_fill_color)
        ),
        na.rm = na.rm
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          color = !!dplyr::sym(var_fill_color)
        ),
        na.rm = na.rm
      )

    # Couleurs du graph par palette
    if (!is.null(palette_color)){
      graph <-
        graph +
        ggplot2::scale_color_brewer(palette = palette_color)
    }

    # Couleurs du graph manuelles
    if (!is.null(manual_color)){
      graph <-
        graph +
        ggplot2::scale_color_manual(values = manual_color)
    }
  }


  # Paramètres graphiques ---------------------------------------------------

  # Définir les breaks sur l'axe des x
  graph <-
    graph +
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    )

  # Exprimer l'axe des ordonnées en pourcentage si souhaité + breaks
  if (percent == TRUE){
    graph <-
      graph +
      # Suppose que les parts de marché sont en pourcentage et non pas proportion
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(scale = 1)
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
      color    = color_legend,
      fill     = fill_legend
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
