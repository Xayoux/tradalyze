# Documentation ------------------------------------------------------------
#' @title Load the Data in Arrow Format
#'
#' @description Take data and load itin arrow format. Data can be a path to
#' a csv or an excel file. It can also be a path to a folder containing
#' parquet files. Dataframe and ArrowObject are also accepted.
#' 
#' @param data Data to be loaded. Can be a path to a csv or an excel file.
#' It can also be a path to a folder containing
#' parquet files. Dataframe and ArrowObject are also accepted.
#' 
#' @return The data opened and loaded in an ArrowObject
#'
#' @examples
#' # Load data of a folder containing parquet files
#' # load_data(here::here("folder-parquet-files"))
#'
#' # Load data of an ArrowObject
#' # here::here("folder-parquet-files")  |>
#' #   arrow::open_dataset() |>
#' #   load_data()
#'
#' # Load data of a csv file
#' # load_data(here::here("csv-file.csv"))
#'
#' @export
# Function -----------------------------------------------------------------
load_data <- function(data){
  data_class <- class(data)
  # Test if data is a path or a dataframe or an arrow object
  if (!is.character(data) & !is.data.frame(data) & !"ArrowObject" %in% data_class){
    stop(stringr::str_glue("\ndata must be a path to a csv or excel file or a path to a parquet folder. It can also be a dataframe or an arrow object.\n\nIt can't be a {data_class}.\n"))
  }

  # If data is a path for a unique file it must be an xlsx or csv file
  if (is.character(data)){
    data_ext <- tools::file_ext(data)
    if (!data_ext %in% c("csv", "xlsx", "")){
      stop(stringr::str_glue("\nIf a path to a unique file is supply, it must be a csv or an xlsx file.\n\nIt can't be a {data_ext} file.\n"))
    }
  } else {
    data_ext <- "NA"
  }
  

  # Unique file + csv -> import csv
  if (length(data_ext) == 1 & data_ext == "csv"){
    df <-
      data |>
      readr::read_csv(show_col_types = FALSE) |>
      arrow::arrow_table()
  }
  # Unique file + xlsx -> import xlsx
  else if (length(data_ext) == 1 & data_ext == "xlsx"){
    df <-
      data  |>
      readxl::read_xlsx() |>
      arrow::arrow_table()
  }
  # If not csv or xlsx and character -> open parquet dataset
  else if (is.character(data)){
    df <-
      data |>
      arrow::open_dataset()
  }
  # dataframe -> convert to an arrow object
  else if (is.data.frame(data)){
    df <-
      data  |>
      arrow::arrow_table()
  }
  # If it's not do nothing bc it's already an arrow object
  else {
    df <- data
  }

  return(df)
}

