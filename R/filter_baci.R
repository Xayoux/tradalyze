# Documentation -------------------------------------------------------------
#' @title Filter the BACI Database
#'
#' @description Keep only wanted data in the BACI database. You can filter by
#' years, product codes or by countries.
#'
#' @details
#' 
#' When filtering by countries ou can filter with the numeric iso codes
#' (variable i/j) or the character iso codes (variable exporter/importer).
#'
#' It is recommended to use the function \code{\link[=dl_baci]{dl_baci}} to
#' obtain the BACI database. This way you are ensured to have good names and
#' types for the BACI variables.
#'
#' The BACI database must have (if you are filtering by it) the following
#' variables :
#'
#'
#' - \strong{t} : Numeric type. Used with `years`.
#' - \strong{k} : Character type. Used with `codes`.
#' - \strong{i} : Numeric type.  Used with `export_countries` with numeric inputs.
#' - \strong{exporter} : Character type. Used with `export_countries` with character inputs.
#' - \strong{j} : Numeric type. Used with `import_countries` with numeric inputs.
#' - \strong{importer} : Character type. Used with `import_countries` with character inputs.
#'
#' Data are loaded in ArrowObject with the function \code{\link[=load_data]{load_data}}.
#' 
#' @param baci Data to be loaded. Can be a path to a csv or an excel file.
#' It can also be a path to a folder containing
#' parquet files. Dataframe and ArrowObject are also accepted.
#' @param years Numeric vector indicating the years to be kept in the variable `t`.
#' @param codes Character vector indicating the product codes to be kept in the
#' variable `k`.
#' @param export_countries Character or numeric vector indicating the
#' exporter countries to be kept in the variable `exporter` or `i`.
#' @param import_countries Character or numeric vector indicating the importer
#' countries to be kept in the variable `importer` or `j`.
#' 
#' @return The BACI database filtered in an ArrowObject.
#'
#' @examples
#' # Keep years from 2015 to 2022
#' # filter_baci(baci = "folder_parquet_files", years = c(2015:2022))
#'
#' # Keep products 420211 and 420212
#' # filter_baci(baci = "folder_parquet_files", codes = c("420211", "420212"))
#'
#' # Keep exporters France and Italy with iso codes character
#' # filter_baci(baci = "folder_parquet_files", export_countries = c("FRA", "ITA"))
#'
#' # Keep exporters France and Italy with iso codes numeric
#' # filter_baci(baci = "folder_parquet_files", export_countries = c(251, 380))
#'
#' @export
# Fonction ------------------------------------------------------------------
## Definition ---------------------------------------------------------------
filter_baci <- function(baci, years = NULL, codes = NULL,
                        export_countries = NULL, import_countries = NULL){

  ## Error messages ---------------------------------------------------------
  # Years must be a numeric
  if (!is.numeric(years)){
    years_type <- class(years)
    stop(stringr::str_glue("\nyears must be a numeric not a {years_type}.\n"))
  }

  # codes must be a numeric
  if (!is.character(codes)){
    codes_type <- class(codes)
    stop(stringr::str_glue("\ncodes must be a character not a {codes_type}.\n"))
  }

  # export_countries must be a numeric or a character
  if (!is.character(export_countries) & !is.numeric(export_countries)){
    export_countries_type <- class(export_countries)
    stop(stringr::str_glue("\nexport_countries must be a character or a numeric, not a {export_countries_type}"))
  }

  # import_countries must be a numeric or a character
  if (!is.character(import_countries) & !is.numeric(import_countries)){
    import_countries_type <- class(import_countries)
    stop(stringr::str_glue("\nimport_countries must be a character or a numeric, not a {import_countries_type}"))
  }

  ## Filter data ------------------------------------------------------------
  # Load the data : loaded in format arrow
  df_baci <- tradalyze::load_data(baci)

  # Filter the years : variable t
  if (!is.null(years)){
    df_baci <-
      df_baci  |>
      dplyr::filter(
        t %in% years
      )
  }

  # Filter the product codes : variable k
  if (!is.null(codes)){
    df_baci <-
      df_baci  |>
      dplyr::filter(
        k %in% codes
      )
  }

  # Filter the exporters : variable i or exporter (depending of the type)
  if (!is.null(export_countries)){
    if (is.character(export_countries)){
      df_baci <-
        df_baci |>
        dplyr::filter(
          exporter %in% export_countries
        )
    } else if (is.numeric(export_countries)){
      df_baci <-
        df_baci  |>
        dplyr::filter(
          i %in% export_countries
        )
    }
  }

  # Filter the importers : variable j or importer (depending of the type)
  if (!is.null(import_countries)){
    if (is.character(import_countries)){
      df_baci <-
        df_baci |>
        dplyr::filter(
          importer %in% import_countries
        )
    } else if (is.numeric(import_countries)){
      df_baci <-
        df_baci  |>
        dplyr::filter(
          j %in% import_countries
        )
    }
  }

  # Return the filtered ArrowObject
  return(df_baci)
}
