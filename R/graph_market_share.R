#' @title
#' Graph of the Evolution of Market Share in Data
#'
#' @description
#' Create, export and save a graph showing the evolution of the market share.
#' (it could be other data, but this function has been designed with the idea
#' of market share in mind). The graph can be an area graph, a line graph or a
#' line and point graph. graph. Some graphic parameters have been set for this
#' function, but all can be manually changed inside or outside the function
#' (the graph can be exported and thus (the graph can be exported and modified
#' after the function).
#'
#' This function does not calculate market share. Make sure you use the market
#' share variable (or the variable you want to represent). It is suggested to
#' use the function \link{market_share} to calculate market shares. 
#'
#' @details
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
#' 
#' @param x Name of the variable used for the x axis. By default it is "t".
#' @param y Name of the variable used for the y axis. 
#' @param graph_type Type of the graph to be represented. It can be "area"
#' (by default and the suggested value) for an area graph, "line" for a line
#' graph or "line_point" for a line and point graph. 
#' @param var_fill_color Name of the variable to be used to color the
#' graph. 
#' @param palette_color Name of the palette to be used. See
#' \link[ggplot2]{scale_fill_brewer}. Be carefull, if the number of colors
#' in the palette is too small compared to the number of categories in
#' the variable `var_fill_color`, an error will be returned.
#' By default this parameter is set on NULL if no palette color is used.
#' @param manual_color Colors to be used for each category. Can be a vector
#' of colors or a list linking each color to a category. By default this variable
#' is NULL if no manual color is used.
#' @param percent Logical indicating if labels on the y axis should be expressed
#' in percentage or not. TRUE is the default value. Ff TRUE data must already
#' be expressed in percentage.
#' @param na.rm Logical indicating whether NA should be exclude of the
#' graph (avoid warning message). TRUE is the default value.
#' @param x_breaks Values of the breaks on the x axis. By default the value is
#' NULL. In this case values are automatically choosen.
#' @param y_breaks Values of the breaks on the y axis. By default the value is
#' NULL. In this case values are automatically choosen.
#' @param x_title Title of the x axis. By default it is set to an empty string.
#' @param y_title Title of the y axis. By default it is set to an empty string.
#' @param title Title of the graph. By default it is set to an empty string.
#' @param subtitle Subtitle of the graph. By default it is set to an empty string.
#' @param caption Caption of the graph. By default it is set to an empty string.
#' @param legend_title Title of the color legend. By default it is set to an
#' empty string.
#' @param type_theme General theme of the graph. It can be "bw" (the default)
#' `ggplot2::theme_bw()`, "classic" for `ggplot2::theme_classic()` or "minimal"
#' for `ggplot2::theme_minimal()`.
#' @param var_facet Name of the variable to be used to divide the graphs in
#' differents part with `ggplot2::facet_wrap()`. By default it his set on NULL
#' indicating that the graph will not be divide. Depends on the level of
#' aggregation of the data and the construction of the data. 
#' @param path_output Path to save the graph in "pdf" or "png" format. If NULL
#' (the default), the graph will not be saved.
#' @param width Width of the saved graph. Bu default it is set to 15
#' @param height Height of the saved graph. By default it is set to 8
#' @param print Logical indicating if the graph should be printed (default)
#' or not.
#' @param return_output Logical indicating if the graph must be returned for
#' further modifications (default) or not.
#' @inheritParams add_chelem_classification
#'
#' @return Graph representing the evolution of market shares.
#'
#' @examples
#' ## graph area printed and saved
#' ## market shares had been computed at the exporter level
#' ## graph_market_share(
#' ##   baci = "df-baci-market-share.csv",
#' ##   x = "t",
#' ##   y = "market_share_v",
#' ##   graph_type = "area",
#' ##   var_fill_color = "exporter",
#' ##   palette_color = "Paired",
#' ##   percent = TRUE,
#' ##   title = "Title of the graph at the exporter level",
#' ##   legend_title = "Exporters",
#' ##   type_theme = "classic",
#' ##   path_output = "path-to-png.png",
#' ##   return_output = FALSE
#' ## )
#'
#' ## graph area returned for modifications
#' ## market shares had been computed at the region level
#' ## One graph for each category of products
#' ## graph_market_share(
#' ##   baci = "df-baci-market-share.csv",
#' ##   x = "t",
#' ##   y = "market_share_v",
#' ##   graph_type = "area",
#' ##   var_fill_color = "region",
#' ##   manual_color = c("blue", "red"),
#' ##   percent = TRUE,
#' ##   type_theme = "classic",
#' ##   var_facet = "category",
#' ##   return_output = TRUE,
#' ##   print = FALSE
#' ## )  +
#' ##   ggplot2::theme(
#' ##     legend.position = "left"
#' ##   )
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' [market_share()] To compute market shares.
#'
#' @export
graph_market_share <- function(baci, x = "t", y,
                               graph_type = c("area", "line", "line_point"),
                               var_fill_color, palette_color = NULL,
                               manual_color = NULL, percent = TRUE,
                               na.rm = TRUE, x_breaks = NULL, y_breaks = NULL,
                               x_title = "", y_title = "", title = "",
                               subtitle = "", caption = "", legend_title = "",
                               type_theme = c("bw", "classic", "minimal"),
                               var_facet = NULL, path_output = NULL,
                               width = 15, height = 8, print = TRUE,
                               return_output = TRUE){


  # Check if x is a character and length 1
  tradalyze::.check_character(x, "x")
  tradalyze::.check_length_1(x, "x")

  # Check if y is character and length 1
  tradalyze::.check_character(y, "y")
  tradalyze::.check_length_1(y, "y")

  # Check if parameter graph_type is valid
  graph_type <- match.arg(graph_type)

  # Check if var_fill_color is character
  tradalyze::.check_character(var_fill_color, "var_fill_color")

  # Check if palette_color is null or character
  tradalyze::.check_null_character(palette_color, "palette_color")

  # Check if manual_color is null or character
  tradalyze::.check_null_list_character(manual_color, "manual_color")

  # Check if percent is logical
  tradalyze::.check_logical(percent, "percent")

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

  # Check if legend_title is character and length 1
  tradalyze::.check_character(legend_title, "legend_title")
  tradalyze::.check_length_1(legend_title, "legend_title")

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
  

  # Create graph ----------------------------------------------------------
  # Load baci data
  df_baci <-
    tradalyze::.load_data(baci) |>
    dplyr::collect()

  # Check if columns `x`, `y`  and `var_fill_color` are present in `df_baci`
  tradalyze::.check_var_exist(df = df_baci, name_df = "df_baci", columns = c(x, y, var_fill_color))


  # Base of the graph
  graph <-
    df_baci |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!dplyr::sym(x),
        y = !!dplyr::sym(y)
      )
    )

  # Type of the graph -----------------------------------------------------

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

    # Fill of the graph by palette
    if (!is.null(palette_color)){
      graph <-
        graph +
        ggplot2::scale_fill_brewer(palette = palette_color)
    }

    # Fill of the graph manually
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

    # Colors of the graph by palette
    if (!is.null(palette_color)){
      graph <-
        graph +
        ggplot2::scale_color_brewer(palette = palette_color)
    }

    # Colors of the graph manually
    if (!is.null(manual_color)){
      graph <-
        graph +
        ggplot2::scale_color_manual(values = manual_color)
    }
  }
  # Graph line_point (lines with point on observations)
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

    # Colors of the graph by palette
    if (!is.null(palette_color)){
      graph <-
        graph +
        ggplot2::scale_color_brewer(palette = palette_color)
    }

    # Couleurs of the graph manually
    if (!is.null(manual_color)){
      graph <-
        graph +
        ggplot2::scale_color_manual(values = manual_color)
    }
  }


  # Graphics parameters ---------------------------------------------------

  # Breaks on x axis
  if (!is.null(x_breaks)){
    graph <-
      graph +
      ggplot2::scale_x_continuous(
        breaks = x_breaks
      )
  }
  

  # Breaks on y axis
  if (!is.null(y_breaks)){
    # Change label of y axis to percent
    if (percent == TRUE){
      graph <-
        graph +
        # Suppose que les parts de marché sont en pourcentage et non pas proportion
        ggplot2::scale_y_continuous(
          labels = scales::label_percent(scale = 1),
          breaks = y_breaks
        )
    }
    # Kepp label of y axis standard
    else {
      graph <-
        graph +
        # Suppose que les parts de marché sont en pourcentage et non pas proportion
        ggplot2::scale_y_continuous(
          breaks = y_breaks
        )
    }
  } else {
    if (percent == TRUE){
      graph <-
        graph +
        # Suppose que les parts de marché sont en pourcentage et non pas proportion
        ggplot2::scale_y_continuous(
          labels = scales::label_percent(scale = 1)
        )
    }
  }

  
  # Titles 
  graph <-
    graph +
    ggplot2::labs(
      x        = x_title,
      y        = y_title,
      title    = title,
      subtitle = subtitle,
      caption  = caption,
      color    = legend_title,
      fill     = legend_title
    )

  # Theme general of the graph
  graph <-
    graph +
    switch(
      type_theme,
      "bw" = ggplot2::theme_bw(),
      "classic" = ggplot2::theme_classic(),
      "minimal" = ggplot2::theme_minimal()
    )


  # Themes ------------------------------------------------------------------

  graph <-
    graph +
    ggplot2::theme(
      # Remove gridlines
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),

      # Titles options
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

      # X axis options
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

      # Y axis options
      axis.text.y =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      axis.title.y =
        ggplot2::element_text(
          size = 22
        ),

      # Legend options
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

      # Facet options
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

  # Facet
  if (!is.null(var_facet)){
    graph <-
      graph +
      ggplot2::facet_wrap(
        c(var_facet),
        scales = "free_y"
      )
  }


  # Export graph -----------------------------------------------------------
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
