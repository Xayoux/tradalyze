% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dot-load_data.R
\name{.load_data}
\alias{.load_data}
\title{Load the Data in Arrow Format}
\usage{
.load_data(data)
}
\arguments{
\item{data}{Data to be loaded. Can be a path to a csv or an excel file.
It can also be a path to a folder containing
parquet files. Dataframe and ArrowObject are also accepted.}
}
\value{
The data opened and loaded in an ArrowObject
}
\description{
Take data and load it in arrow format. Data can be a path to
a csv or an excel file. It can also be a path to a folder containing
parquet files. Dataframe and ArrowObject are also accepted.
}
\examples{
# Load data of a folder containing parquet files
# .load_data(here::here("folder-parquet-files"))

# Load data of an ArrowObject
# here::here("folder-parquet-files")  |>
#   arrow::open_dataset() |>
#   .load_data()

# Load data of a csv file
# .load_data(here::here("csv-file.csv"))

}
