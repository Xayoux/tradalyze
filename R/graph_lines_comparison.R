#' @title
#' Create Line Graph
#'
#' @description
#' Represent a line graph allowing to represent the evolution of
#' a variable. 
#'
#' @details
#' # General informations
#' This function don't modify data, so you must provide exact data you want
#' to represent.
#'
#' Thanks to the `return_output` parameter, it is possible to return the graph,
#' allowing for further modifications.
#'
#' # Theme default parameters
#' ## Grid lines
#' Major and minor gridlines are removed (`panel.grid.minor()` and
#' `panel.grid.major()`).
#'
#' ## Titles options
#' - The size of the title is 26 and it is centered.
#'
#' - the size of the subtitle is 22 and it is centered.
#'
#' - The size of the caption is 16 and it is left adjust.
#'
#' ## x axis options
#' - The text of the x axis if at an angle of 45° and tight adjust. It has a
#' size of 18 and a "black" color.
#'
#' - The size of the x axis title is 22 and `vjust = -0.5`.
#'
#' ## y axis options
#' - The size of the y axis text is 18 and its color is "black".
#'
#' - the size of the y axis title is 22.
#'
#' ## Legend options
#' - legend position is right
#'
#' - Text of the legend has a size of 18 and a "black" color.
#'
#' - `legend.key.spacing.y = ggplot::unit(0.3, "cm")`.
#'
#' - the title of the legend has a size of 22, a "black" color and is centered.
#'
#' ## Facet options
#' - The strip of the facet has a "black" colour and is fill with the following
#' color : `"#D9D9D9"`.
#'
#' - The text of the strip has a size of 18 and a "black" color.
#'
#' By default facet are "free_y".
#'
#' @param linewidth Numeric indicating the width of the lines. By default it is
#' set to 1.
#' @param var_linetype Character indicating the name of the variable use
#' to define the type of the lines. By default it is set to NULL : all the
#' lines will be the same.
#' @param manual_linetype Character indicating the linetype to be used for each category.
#' Can be a vector of linetype or a list linking each linetype to a category.
#' By default this variable is NULL if no manual linetype is used. Used only
#' if `var_linetype = TRUE`.
#' @param var_color Name of the variable to be used to color the
#' lines. By default it is set to null indicating that all lines will have the
#' same default color.
#' @param palette_color Name of the palette to be used. See
#' \link[ggplot2]{scale_fill_brewer}. Be carefull, if the number of colors
#' in the palette is too small compared to the number of categories in
#' the variable `var_color`, an error will be returned.
#' @param color_legend Title of the legend for the colors. By default it is
#' set to "".
#' @param linetype_legend Title of the legend for the linetype. By default it
#' is set to "". if the variable for the linetype and the color is the same,
#' there will only be one legend.
#' @inheritParams add_chelem_classification
#' @inheritParams graph_market_share
#'
#' @return Graph representing the time evolution of a variable.
#'
#' @examples
#' ## Basic example
#' ## graph_lines_comparison(
#' ##   baci = "data-folder-parquet",
#' ##   x = "t",
#' ##   y = "adressed-demand",
#' ##   linewidth = 1,
#' ##   var_linetype = "exporter",
#' ##   var_color = exporter,
#' ##   palette_color = "Paired",
#' ##   type_theme = "minimal",
#' ##   var_facet = "category"
#' ## )
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' 
#' @export
graph_lines_comparison <- function(baci, x, y, linewidth = 1,
                                  var_linetype = NULL, manual_linetype = NULL,
                                  var_color = NULL, palette_color = NULL,
                                  manual_color = NULL,
                                  na.rm = TRUE, x_breaks = NULL, y_breaks = NULL,
                                  x_title = "", y_title = "", title = "",
                                  subtitle = "", caption = "", color_legend = "",
                                  linetype_legend ="", type_theme = c("bw", "classic", "minimal"),
                                  var_facet = NULL, path_output = NULL,
                                  width = 15, height = 8, print = TRUE,
                                  return_output = TRUE){

  # Check if x is a character and length 1
  tradalyze::.check_character(x, "x")
  tradalyze::.check_length_1(x, "x")

  # Check if y is character and length 1
  tradalyze::.check_character(y, "y")
  tradalyze::.check_length_1(y, "y")

  # Check if linewidth is numeric and length 1
  tradalyze::.check_numeric(linewidth, "linewidth")
  tradalyze::.check_length_1(linewidth, "linewidth")

  # Check if var_linetype is character or NULL
  tradalyze::.check_null_character(var_linetype, "var_linetype")

  # Check if manual_linetype is null or character or a list
  tradalyze::.check_null_list_character(manual_linetype, "manual_linetype")

  # Check if var_color is character or NULL
  tradalyze::.check_null_character(var_color, "var_color")

  # Check if palette_color is character or NULL
  tradalyze::.check_null_character(palette_color, "palette_color")

  # Check if manual_color is null or character or a list
  tradalyze::.check_null_list_character(manual_color, "manual_color")

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

  # Check if x_breaks is NULL or numeric
  tradalyze::.check_null_numeric(x_breaks, "x_breaks")

  # Check if y_breaks is NULL or numeric
  tradalyze::.check_null_numeric(y_breaks, "y_breaks")

  # Check if x_title is character and length 1
  tradalyze::.check_character(x_title, "x_title")
  tradalyze::.check_length_1(x_title, "x_title")

  # Check if y_title is character and length 1
  tradalyze::.check_character(y_title, "y_title")
  tradalyze::.check_length_1(y_title, "y_title")

  # Check if title is character and length 1
  tradalyze::.check_character(title, "title")
  tradalyze::.check_length_1(title, "title")

  # Check if subtitle is character and length 1
  tradalyze::.check_character(subtitle, "subtitle")
  tradalyze::.check_length_1(subtitle, "subtitle")

  # Check if caption is character and length 1
  tradalyze::.check_character(caption, "caption")
  tradalyze::.check_length_1(caption, "caption")

  # Check if color_legend is character and length 1
  tradalyze::.check_character(color_legend, "color_legend")
  tradalyze::.check_length_1(color_legend, "color_legend")

  # Check if linetype_legend is character and length 1
  tradalyze::.check_character(linetype_legend, "linetype_legend")
  tradalyze::.check_length_1(linetype_legend, "linetype_legend")

  # Check if parameter type_theme is valid
  type_theme <- match.arg(type_theme)

  # Check if var_facet is NULL or character
  tradalyze::.check_null_character(var_facet, "var_facet")

  # Check if path_output is NULL or character
  tradalyze::.check_null_character(path_output, "path_output")

  # Check if extension of path_output is pdf or png
  ext_path_output <- tools::file_ext(path_output)
  if (!is.null(path_output)){
    if (!ext_path_output %in% c("png", "pdf")){
      stop(glue::glue("If path output is provided, the extension must be \"png\" or \"pdf\", not \"{extpath_output}\"."))
    }
  }

  # Check if width is numeric and length 1
  tradalyze::.check_numeric(width, "width")
  tradalyze::.check_length_1(width, "width")

  # Check if height is numeric and length 1
  tradalyze::.check_numeric(height, "height")
  tradalyze::.check_length_1(height, "height")

  # Check if print is logical and length 1
  tradalyze::.check_logical(print, "print")
  tradalyze::.check_length_1(print, "print")

  # Check is return_output is logical and length 1
  tradalyze::.check_logical(return_output, "return_output")
  tradalyze::.check_length_1(return_output, "return_output")


  # Load data
  df_baci <-
    tradalyze::.load_data(baci)  |>
    dplyr::collect()

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
  if (!is.null(var_color) & !is.null(var_linetype)){
    if (var_color == var_linetype){
      graph <-
        graph +
        ggplot2::guides(
          color = ggplot2::guide_legend(title = color_legend),
          linetype = ggplot2::guide_legend(title = linetype_legend)
        )
    }
  }


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

  # Themes 
  graph <-
    graph +
    switch(
      type_theme,
      "bw" = ggplot2::theme_bw(),
      "classic" = ggplot2::theme_classic(),
      "minimal" = ggplot2::theme_minimal()
    )

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

  # Export graph
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
