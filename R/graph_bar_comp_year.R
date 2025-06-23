# Documentation -------------------------------------------------------------
#' @title
#' Create Bar Graph to Compare Two Years
#'
#' @description
#' Represent a bar graph allowing to compare the level and evolution of
#' different groups in a variable.
#'
#' @details
#' # Representations
#' There is two types of representation. The first is by stacking the bar
#' with `double_bar = TRUE`. The `year_1` is represented by a darker bar and the
#' `year_2` is represented by a clear bar above. Be carefull with this
#' representation because if the value of `year_1` is greter than the value
#' of `year_2` the clearer bar will be hidden by the darker bar and only the
#' outline will be visible if `color_2` is provided.
#'
#' The second representation consist of one bar representing the `year_1` and
#' the `year_2` is representing by a shape. 
#'
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
#' @param x Name of the variable used for the x axis.
#' @param y Name of the variable used for the y axis.
#' @param double_bar Logical indicating whether bar must be stacked (TRUE), with
#' darker indicating the `year_1` and the clearer indicating the `year_2`, or
#' not (FALSE : the default value). In this case `year_2` is indicating by a shape.
#' @param var_t Character indicating the name of the temporal variable
#' from wich `year_1` and `year_2` are extracted.
#' @param year_1 Numeric indicating the year 1.
#' @param year_2 Numeric indicating the year 2.
#' @param color_1 Character indicating the color of the `year_1` bar for the
#' outline. By default it is set to FALSE indicating that there will be no
#' outline. 
#' @param color_2 Character indicating the color of the `year_2` bar for the
#' outline. By default it is set to FALSE indicating that there will be no
#' outline. Not used if `doubel_bar = TRUE`.
#' @param alpha Numeric indicating the transparency of the `year_2` bar or
#' the shape representing `year_2` depending on `double_bar` parameter. By
#' default it is set to 0.7. It is not recommended to put it to 1. 
#' @param var_fill Character indicating the variable to be used to color
#' the bars (the two bars in the case of `double_bar = TRUE` or the unique bar
#' if `double_bar = FALSE`). If NULL (the default) is provided, the basic color
#' will be used.
#' @param palette_fill Character indicating the name of the palette to be used. See
#' \link[ggplot2]{scale_fill_brewer}. Be carefull, if the number of colors
#' in the palette is too small compared to the number of categories in
#' the variable `var_fill_color`, an error will be returned.
#' By default this parameter is set on NULL if no palette color is used.
#' @param manual_fill Character indicating the colors to be used for each category.
#' Can be a vector of colors or a list linking each color to a category.
#' By default this variable is NULL if no manual color is used. These color
#' applicates also to the shape if `var_fill_shape` if used.
#' @param shape Numeric corresponding to the number of the shape wanted
#' if `double_bar = FALSE`. By default it is set to 22 (a full square). It is
#' recommended to use shape above or equal to 21 beacause these shape are full
#' and can be colored. 
#' @param size_shape Numeric indicating the
#' size of the shape if `double_bar = TRUE`. By default it is set to 5.
#' @param var_fill_shape Character indicating the name of the variable
#' to be used to color the shape if `double_bar = TRUE`. It is recommended to
#' use the same as `var_fill`. If NULL (the default) is provided you can
#' use `fill_shape` to set a unique color. 
#' @param fill_shape Character indicating the color to be used for the shape
#' if `double_bar = TRUE` and `var_fill_shape = NULL`. The color will be the
#' same for all shapes. The default is "black".
#' @inheritParams add_chelem_classification
#' @inheritParams graph_market_share
#' 
#' @return Graph representing the level and evolution of a variable for 2 years
#'
#' @examples
#' ## Bar stacked
#' ## graph_bar_comp_year(
#' ##   baci = "data-folder-parquet",
#' ##   x = "exporter",
#' ##   y = "uv",
#' ##   double_bar = TRUE,
#' ##   var_t = "t",
#' ##   year_1 = 2010,
#' ##   year_2 = 2022,
#' ##   color_1 = "black",
#' ##   color_2 = "black",
#' ##   alpha = 0.7,
#' ##   var_fill = "exporter",
#' ##   palette_fill = "Paired",
#' ##   title = "Compare the level and evolution of unit values between 2010 and 2022",
#' ##   type_theme = "bw",
#' ##   var_facet = "sector"
#' ## ) +
#' ##   ggplot2::theme(
#' ##     legend.position = "none" # No legend display
#' ##   )
#'
#' ## Bar and shape
#' ## graph_bar_comp_year(
#' ##   baci = "data-folder-parquet",
#' ##   x = "exporter",
#' ##   y = "quality",
#' ##   double_bar = FALSE,
#' ##   var_t = "t",
#' ##   year_1 = 2010,
#' ##   year_2 = 2022,
#' ##   alpha = 0.7,
#' ##   var_fill = "exporter",
#' ##   palette_fill = "Paired",
#' ##   shape = 21,
#' ##   size_shape = 6,
#' ##   var_fill_shape = "exporter"
#' ##   title = "Compare the level and evolution of unit values between 2010 and 2022",
#' ##   type_theme = "classic"
#' ## ) +
#' ##   ggplot2::theme(
#' ##     legend.position = "none" # No legend display
#' ##   )
#'
#' @seealso
#' [.load_data()] For more informations concerning the loading.
#' 
#' @export
graph_bar_comp_year <- function(baci, x, y, double_bar = FALSE,
                                var_t,
                                year_1, year_2, color_1 = FALSE,
                                color_2 = FALSE, alpha = 0.7,
                                var_fill = NULL, palette_fill = NULL,
                                manual_fill = NULL, shape = 22, size_shape = 5,
                                var_fill_shape = NULL,
                                fill_shape = "black", na.rm = TRUE,
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

  # Check if double_bar is logical and length 1
  tradalyze::.check_logical(double_bar, "double_bar")
  tradalyze::.check_length_1(double_bar, "double_bar")

  # Check if var_t is character and length 1
  tradalyze::.check_character(var_t, "var_t")
  tradalyze::.check_length_1(var_t, "var_t")

  # Check if year_1 is numeric and length 1
  tradalyze::.check_numeric(year_1, "year_1")
  tradalyze::.check_length_1(year_1, "year_1")

  # Check if year_2 is numeric and length 1
  tradalyze::.check_numeric(year_2, "year_2")
  tradalyze::.check_length_1(year_2, "year_2")

  # Check if alpha is numeric and length 1
  tradalyze::.check_numeric(alpha, "alpha")
  tradalyze::.check_length_1(alpha, "alpha")

  # Check if var_fill is character or NULL
  tradalyze::.check_null_character(var_fill, "var_fill")

  # Check if palette_fill is character or NULL
  tradalyze::.check_null_character(palette_fill, "palette_fill")

  # Check if manual_fill is null or character or a list
  tradalyze::.check_null_list_character(manual_fill, "manual_fill")

  # Check if shape is numeric and length 1
  tradalyze::.check_numeric(shape, "shape")
  tradalyze::.check_length_1(shape, "shape")

  # Check if size_shape is numeric and length 1
  tradalyze::.check_numeric(size_shape, "size_shape")
  tradalyze::.check_length_1(size_shape, "size_shape")

  # Check if var_fill_shape is character or NULL
  tradalyze::.check_null_character(var_fill_shape, "var_fill_shape")

  # Check if fill_shape is character and length 1
  tradalyze::.check_character(fill_shape, "fill_shape")
  tradalyze::.check_length_1(fill_shape, "fill_shape")

  # Check if na.rm is logical and length 1
  tradalyze::.check_logical(na.rm, "na.rm")
  tradalyze::.check_length_1(na.rm, "na.rm")

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
  


  # Open Data
  df_baci <-
    tradalyze::.load_data(baci) |>
    dplyr::collect()

  # Foundations of the graph
  graph <- ggplot2::ggplot()

  # Data for year 1 (make one df for each year : allow multiple bars easily)
  df_baci_year_1 <-
    df_baci |>
    dplyr::filter(!!dplyr::sym(var_t) == year_1)

  # Data for year 2
  df_baci_year_2 <-
    df_baci |>
    dplyr::filter(!!dplyr::sym(var_t) == year_2)

  # TRUE means that bar are stacked together the clear bar is the year 2
  # No visible if year 2 < year 1
  if (double_bar == TRUE){
    graph <-
      graph +
      # First bar (darker)
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
      ) +
      # Second bar (clearer)
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
  }
  # FALSE means one bar and year 2 represent by a share
  else if (double_bar == FALSE){
    graph <-
      graph +
      # Bar for the year 1
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

    # If colors of shape are defined by a variable
    if (!is.null(var_fill_shape)){
      # Points for the year 2
      graph <-
        graph +
        ggplot2::geom_point(
          ggplot2::aes(
            x = !!dplyr::sym(x),
            y = !!dplyr::sym(y),
            fill = !!dplyr::sym(var_fill_shape)
          ),
          alpha = alpha,
          na.rm = na.rm,
          shape = shape,
          size = size_shape,
          data = df_baci_year_2,
          color = "black"
        )
    }
    # If color of the point is the same and define by a character
    else{
      # Point for the second year
      graph <-
        graph +
        ggplot2::geom_point(
          ggplot2::aes(
            x = !!dplyr::sym(x),
            y = !!dplyr::sym(y)
          ),
          na.rm = na.rm,
          shape = shape,
          size = size_shape,
          fill = fill_shape,
          data = df_baci_year_2
        )
    } 
  }

  # Define colors of the bars by a variable
  if (!is.null(var_fill)){
    graph <-
      graph +
      ggplot2::aes(
        fill = !!dplyr::sym(var_fill)
      )

    # Define colors with a palette
    if (!is.null(palette_fill)){
      graph <-
        graph +
        ggplot2::scale_fill_brewer(
          palette = palette_fill
        )
    }

    # Define colors manually
    else if (!is.null(manual_fill)){
      graph <-
        graph +
        ggplot2::scale_fill_manual(
          values = manual_fill
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
      fill     = legend_title
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
      # remove gridlines
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

      # Axis x options
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

      # Axis y options
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
