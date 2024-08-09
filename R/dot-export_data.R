#' @title Export Data (Return and/or Save)
#'
#' @description Save data in parquet ore csv format and/or return data in
#' R dataframe format or \link{arrow} format.
#' 
#' @param data Data to be export. Must be a R dataframe. Can be NULL if
#' `eval = FALSE` to just check the validity of the parameters.
#' @param return_output Logical indicating whether data must be returned or not.
#' By default data are returned after this function. 
#' @param return_arrow Logical indicating whether data must be return in an
#' arrow format (TRUE the default) or not if `return_output = TRUE`.
#' By default data are returned to arrow format.
#' @param path_output Path to save the data. If NULL (default), the data
#' will not be saved. If `path_output` ends with a '.csv' extension, the data
#' will be saved in csv format. If no extension is given, the data will be
#' saved in a dataset parquet format in the specified folder. See the
#' \link[arrow]{arrow} package.  
#' @param eval Logical indicating if the save/return part must be execute. If
#' FALSE, only the check validity or the parmaters will be performed.
#' @param collect A logical indicating if the data has been collected (TRUE) or
#' not (FALSE). 
#' 
#' @return Data saved and/or returned (or nothing).
#'
#' @examples
#' ## Return data in arrow format. No save. Data had been collected
#' ## .export_data(
#' ##   data = df_data,
#' ##   return_output = TRUE,
#' ##   return_arrow = TRUE,
#' ##   eval = TRUE
#' ##   collect = TRUE
#' ## )
#'
#' ## Return data in R dataframe format. Save in parquet format
#' ## .export_data(
#' ##   data = df_data,
#' ##   return_output = TRUE,
#' ##   return_arrow = FALSE,
#' ##   path_output = "folder-data-parquet",
#' ##   eval = TRUE,
#' ##   collect = TRUE
#' ## )
#'
#' ## Don't return data. Save in csv format. data had not been collected
#' ## .export_data(
#' ##   data = df_data,
#' ##   return_output = FALSE,
#' ##   return_arrow = FALSE,
#' ##   path_output = "file-data.csv",
#' ##   eval = TRUE,
#' ##   collect = FALSE
#' ## )
#'
#' @export

.export_data <- function(data = NULL, return_output = TRUE, return_arrow = TRUE,
                         path_output = NULL, eval, collect = NULL){

  # Check if return_output is logical
  if (!is.logical(return_output)){
    class_return_output <- class(return_output)
    stop(glue::glue("return_output must be a logical, not a {class_return_output}."))
  }

  # Check if return_output is length 1
  length_return_output <- length(return_output)
  if (length_return_output != 1){
    stop(glue::glue("return_output must be length 1, not length {length_return_output}."))
  }

  # Check if return_arrow is logical
  if (!is.logical(return_arrow)){
    class_return_arrow <- class(return_arrow)
    stop(glue::glue("return_arrow must be a logical, not a {class_return_arrow}."))
  }

  # Check if return_output is length 1
  length_return_arrow <- length(return_arrow)
  if (length_return_arrow != 1){
    stop(glue::glue("return_arrow must be length 1, not length {length_return_arrow}."))
  }

  # Set return_arrow on FALSE if return_output is FALSE
  if (return_output == FALSE & return_arrow == TRUE){
    message("return_arrow is set on TRUE while return_output is set on FALSE. return_arrow will be ignored.")
    return_arrow <- FALSE
  }

  # Check if path_output is NULL or character
  if (!is.null(path_output) && !is.character(path_output)){
    class_path_output <- class(path_output)
    stop(glue::glue("path_output must be NULL or a character, not a {class_path_output}."))
  }

  # Check if path_output is length 1
  length_path_output <- length(path_output)
  if (is.character(path_output) && length_path_output != 1){
    stop(glue::glue("path_output must be length 1, not length {length_path_output}."))
  }

  # Check if path_output has an extension it is a csv extension
  if (is.character(path_output)){
    ext_path_output <- tools::file_ext(path_output)
    if (!ext_path_output %in% c("", "csv")){
      stop(glue::glue("If an extension is provided to path_output, it must be a \"csv\" extension, not a \"{ext_path_output}\" extension."))
    }
  }


  if (eval == TRUE){
    # if data is in R dataframe format ie: collected
    if (collect == TRUE){
      # Save output if needed in the right format
      if (!is.null(path_output)){
        # Save in csv format
        if (tools::file_ext(path_output) == "csv"){
          rlang::check_installed("readr", reason = "\n\nNecessary to write in csv format.")
          data  |>
            readr::write_csv(path_output)
        }
        # If no extension : save in parquet format
        else {
          data |>
            arrow::arrow_table() |> # passage in arrow format to write in parquet
            dplyr::group_by(t) |>
            arrow::write_dataset(path_output)
        }
      }

      # Return output if needed
      if (return_output == TRUE){
        # If return an arrow object and df_baci has been collect : go to arrow format
        if (return_arrow == TRUE){
          return(data |> arrow::arrow_table()) 
        }
        # Else return df_baci collected or not
        else {
          return(data)
        }
      }
    }
    # If data is in arrow format ie : not collected
    else if (collect == FALSE){
      if (!is.null(path_output)){
        # Save in csv format
        if (tools::file_ext(path_output) == "csv"){
          rlang::check_installed("readr", reason = "\n\nNecessary to write in csv format.")
          data  |>
            dplyr::collect() |>
            readr::write_csv(path_output)
        }
        # If no extension : save in parquet format
        else {
          data |>
            dplyr::group_by(t) |>
            arrow::write_dataset(path_output)
        }
      }

      # Return output if needed
      if (return_output == TRUE){
        # If return an arrow object and df_baci has been collect : go to arrow format
        if (return_arrow == TRUE){
          return(data) 
        }
        # Else return df_baci collected or not
        else {
          return(data  |> dplyr::collect())
        }
      }
    }
  }
}

utils::globalVariables(c("t"))
