# Documentation -------------------------------------------------------------
#' @title
#' Graphique de comparaison en barres
#'
#' @description
#' Représentation d'un graphique en barre permettant de comparer le niveau et
#' l'évolution d'une variable sur deux (ou plus) années différentes.
#'
#' @details
#' Ce graphique permet de comparer le niveau et l'évolution d'une variable
#' sur deux années différentes. Il est possible de représenter les données
#' d'une façon telle qu'une barre représente une année. Il est également
#' possible (ne fonctionne que pour deux années) de représenter les données
#' sur une seule barre. La partie "foncée" indique le niveau à l'année 1, tandis
#' que la différence avec l'année 2 est représenté par la partie "claire".
#'
#' CEtte deuxième représentation fonctionne particlièrement bien pour les
#' données qui ne sont que croissantes ou décroissantes (dans ce dernier cas,
#' il faut intervertir l'année 1 et 2). En effet, si les données ne sont pas
#' uniquement croissantes ou décroissantes, la partie "foncée" causera un
#' overlap avec la partie "claire" et la lecture des données sera difficile.
#' Cela peut être compensé par l'ajout des couleurs dans le paramètre `color`
#' qui permettent de délimiter chaque barre.
#'
#' Grâce au paramètre `return_output = TRUE`, il est possible de retourner le
#' graphique afin de pouvoir le modifier manuellement par la suite.
#'
#'
#' @param baci Chemin d'accès, dataframe ou format parquet des données de baci
#' à utiliser.
#' @param x Variable x du graphique
#' @param y Variable y du graphique
#' @param stack Booléen indiquant si les barres doivent être empilées ou non.
#' Si TRUE, alors la comparaison ne peut se faire qu'entre deux années et les
#' paramètres `year_1` et `year_2` doivent être définis.
#' @param double_bar Booléen indiquant si dans le cas où `stack = TRUE`, les
#' deux barres doivent êtres représentées, ou bien si une seule année doit être
#' représentée sous forme de barre et la deuxième sous forme de point sur la
#' barre. 
#' @param var_t Variable temporelle à utiliser pour la comparaison des années.
#' @param year_1 Année 1 à comparer.
#' @param year_2 Année 2 à comparer.
#' @param color_1 Couleur de la barre de l'année 1. Si FALSE la barre 1
#' (correspondant à l'année 1) n'aura pas de couleurs de bordure. Dans le cas
#' où `stack = FALSE`, ce sera cette couleur qui sera utilisée pour la couleur
#' des bordures des différentes barres.
#' @param color_2 Couleur de la barre de l'année 2. Si FALSE la barre 2
#' (correspondant à l'année 2) n'aura pas de couleurs de bordure.
#' @param alpha Transparence de la barre de l'année 2. Cette transparence
#' permet de voir la barre de l'année 1 à travers la barre de l'année 2.
#' @param var_fill Variable à utiliser pour le remplissage des barres.
#' @param palette_fill Palette de couleur à utiliser pour le remplissage des
#' barres.
#' @param manual_fill Les couleurs à utliser définies manuellement.
#' @param shape Numéros correspond à la formevoulue dans le cas où
#' `stack = TRUE` et `double_bar = FALSE`
#' @param size_shape = la taille de la forme utilisée.
#' @param fill_shape = la couleur de la forme à utiliser. Couleur indentique
#' pour toutes les formes. 
#' @param na.rm Booléen indiquant si les valeurs manquantes doivent être
#' retirée.
#' @param x_title Titre de l'axe des X.
#' @param y_title Titre de l'axe des Y.
#' @param title Titre du graphique.
#' @param subtitle Sous-titre du graphique.
#' @param caption Légende du graphique.
#' @param fill_legend Légende du remplissage des barres.
#' @param type_theme Thème du graphique. Peut être "bw" (noir et blanc),
#' "classic" (classique) ou "minimal" (minimaliste).
#' @param var_facet Variable à utiliser pour les facettes.
#' @param path_output Chemin d'accès pour enregistrer le graphique.
#' @param width Largeur du graphique.
#' @param height Hauteur du graphique.
#' @param print Booléen indiquant si le graphique doit être affiché.
#' @param return_output Booléen indiquant si le graphique doit être retourné.
#' @param var_fill_shape Variable sous forme de chaîne de caractère, servant
#' à donner la couleurs aux points si `double_bar = TRUE`. Si ce paramètre est
#' `FALSE`, alors la couleur sera la même pour tous
#' 
#' @return Un graphique en barre comparant deux années.
#' @export
#'
#' @examples # Pas d'exemples.
# Fonction graph_bar_comp_year ---------------------------------------------
## Définition de la fonction -----------------------------------------------
graph_bar_comp_year <- function(baci, x, y, stack = TRUE, double_bar = FALSE,
                                var_t = NULL,
                                year_1 = NULL, year_2 = NULL, color_1 = FALSE,
                                color_2 = FALSE, alpha = 0.7,
                                var_fill = NULL, palette_fill = NULL,
                                manual_fill = NULL, shape = 22, size_shape = 5,
                                var_fill_shape = NULL,
                                fill_shape = "black", na.rm = TRUE,
                                x_title = "", y_title = "", title = "",
                                subtitle = "", caption = "", fill_legend = "",
                                type_theme = "bw",
                                var_facet = NULL, path_output = NULL,
                                width = 15, height = 8, print = TRUE,
                                return_output = TRUE){


  df_baci <-
    tradalyze::.load_data(baci)  |>
    dplyr::collect()


  # Création du corps du graphique ------------------------------------------
  # Fondations du graph
  graph <- ggplot2::ggplot()

  
  # Une seule barre : évolution montrée par la partie plus claire
  
  # Données pour l'année 1
  df_baci_year_1 <-
    df_baci |>
    dplyr::filter(!!dplyr::sym(var_t) == year_1)

  # Données pour l'année 2
  df_baci_year_2 <-
    df_baci |>
    dplyr::filter(!!dplyr::sym(var_t) == year_2)


  graph <-
    graph +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = !!dplyr::sym(x),
        y = !!dplyr::sym(y)
      ),
      na.rm = na.rm,
      stat = "identity",
      position = "dodge",
      color = color_1,
      data = df_baci_year_1
    )

  if (double_bar == TRUE){
    graph <-
      graph +
      ggplot2::geom_bar(
        ggplot2::aes(
          x = !!dplyr::sym(x),
          y = !!dplyr::sym(y)
        ),
        na.rm = na.rm,
        stat = "identity",
        position = "dodge",
        color = color_2,
        alpha = alpha,
        data = df_baci_year_2
      )
  } else {
    graph <-
      graph +
      ggplot2::geom_point(
        ggplot2::aes(
          x = !!dplyr::sym(x),
          y = !!dplyr::sym(y),
          fill = dplyr::if_else(is.null(var_fill_shape), NULL, !!dplyr::sym(var_fill_shape))
        ),
        alpha = alpha,
        na.rm = na.rm,
        shape = shape,
        size = size_shape,
        data = df_baci_year_2,
        color = "black"
      )
  }
  


  # Définir les couleurs si souhaité
  if (!is.null(var_fill)){
    graph <-
      graph +
      ggplot2::aes(
        fill = !!dplyr::sym(var_fill)
      )

    # Définir la palette de couleur si souhaitée
    if (!is.null(palette_fill)){
      graph <-
        graph +
        ggplot2::scale_fill_brewer(
          palette = palette_fill
        )
    }

    # Définir les couleurs manuellement
    else if (!is.null(manual_fill)){
      graph <-
        graph +
        ggplot2::scale_fill_manual(
          values = manual_fill
        )
    }
  }

  # Paramètres graphiques ---------------------------------------------------

  # Titres et légendes
  graph <-
    graph +
    ggplot2::labs(
      x        = x_title,
      y        = y_title,
      title    = title,
      subtitle = subtitle,
      caption  = caption,
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

iris |>
  ggplot2::ggplot(ggplot2::aes(x = Sepal.Length, y = Sepal.Width, color = Species, fill = Species))+
  ggplot2::geom_point() +
  ggplot2::scale_color_brewer(palette = "Paired")
