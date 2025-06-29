% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_compare.R
\name{aggregate_compare}
\alias{aggregate_compare}
\title{Aggregate a variable and Compare it in Base 100.}
\usage{
aggregate_compare(
  baci,
  years = NULL,
  codes = NULL,
  export_countries = NULL,
  import_countries = NULL,
  method = c("mean", "median", "weighted_mean", "weighted_median"),
  var,
  var_aggregation = c("t", "k", "exporter"),
  var_temporal = "t",
  var_weight = NULL,
  fixed_weight = FALSE,
  year_ref_fixed_weight = NULL,
  var_disaggregation = c("t", "exporter", "importer", "k"),
  base_100 = FALSE,
  year_ref_base_100 = NULL,
  compare = FALSE,
  var_exporter = NULL,
  exporter_ref = NULL,
  na.rm = TRUE,
  return_output = TRUE,
  return_arrow = TRUE,
  path_output = NULL
)
}
\arguments{
\item{baci}{BACI data. Can be a path to a csv file.
It can also be a path to a folder containing
parquet files. Dataframe and ArrowObject are also accepted. Xlsx files are
also accepted, but absolutely not recommended because BACI data are too large
and data must be in the first sheet.}

\item{years}{Numeric vector indicating the years to be kept in the variable \code{t}.}

\item{codes}{Character vector indicating the product codes to be kept in the
variable \code{k}.}

\item{export_countries}{Character or numeric vector indicating the
exporter countries to be kept in the variable \code{exporter} or \code{i}.}

\item{import_countries}{Character or numeric vector indicating the importer
countries to be kept in the variable \code{importer} or \code{j}.}

\item{method}{Character indicating the method to be used to aggregate
the choosen variable. It can be "mean" (the default) to compute the mean ;
"median" to compute the median ; "weighted_mean" to commpute a weighted mean
(with fixed weight or not) ; "weighted_median" to compute a weighted median
(with fixed weight or not).}

\item{var}{Character indicating the name of the variable to be aggregated.}

\item{var_aggregation}{Character vector containing the name of variables used
as a group to aggregate the data. Example : if \code{var_aggregation = c("t", "k", "exporter")}, the variable will be aggregated at the time, product, exporter
level.}

\item{var_temporal}{Character indicating the name of the temporal
variable in the data. Generally it will be "t".}

\item{var_weight}{Character indicating the name of the variable used
as weight. Used only if \code{method = "weighted_mean"} or
\code{method = "weighted_median"}. If not used it can be set to NULL.}

\item{fixed_weight}{Logical indicating whether the weight should be kept fixed (TRUE) or
not (FALSE : the default). Used only if \code{method = "weighted_mean"} or
\code{method = "weighted_median"}. If TRUE, the weight will be the same each year
and will correspond to the weight of the year indicating in \code{year_ref_fixed_weight}.}

\item{year_ref_fixed_weight}{Numeric indicating the year of reference
for fixed weight. Used only if \code{fixed_weight = TRUE} and \code{method = "weighted_mean"}
or \code{method = "weighted_median"}. If not used it can be set to NULL.}

\item{var_disaggregation}{Character vector indicating the name variables
corresponding of the lower level of aggregation possible in the data.
Generally, if BACI is taken, it will be
\code{var_disaggregation = c("t", "exporter", "importer", "k")}. Used only
if \code{fixed_weight = TRUE}.}

\item{base_100}{Logical indicating whether the aggregated variable must be
transformed in base 100 (TRUE) or not (FALSE : the default). If TRUE two new
variables will be added :
\describe{
\item{'var'_year_ref}{Numeric : Value of the aggregate variable in the
reference year defined by \code{year_ref_base_100} parameter. The name of the variable
depends on the name the aggregated variable. If the aggregated variable
is "uv", the name of this variable will be "uv_year_ref".}
\item{'var_100'}{Numeric : Value of the aggregated variable transformed
in base 100. The name of the variable depends on the name the aggregated
variable. If the aggregated variable is "uv", the name of this variable will
be "uv_year_ref".}
}}

\item{year_ref_base_100}{Numeric indicating the reference year at which the
base 100 begin. Use only if \code{base_100 = TRUE}. If not used, it can be set on
NULL.}

\item{compare}{Logical indicating if the base 100 should be compared (TRUE)
by a ratio, to the base 100 of a reference exporter, defined by the parameter
\code{exporter_ref}, or not (FALSE the default). If TRUE, two nex variables are
added
\describe{
\item{'var'_exporter_ref}{Numeric : Value of the aggregate variable for the
reference exporter defined by \code{exporter_ref} parameter. The name of the variable
depends on the name the aggregated variable. If the aggregated variable
is "uv", the name of this variable will be "uv_exporter_ref".}
\item{'var_100_diff'}{Numeric : Value of the ratio between the base 100 and
the base 100 of the reference exporter. A value greater than 1 indicate that
the the variable for the exporter has increased more than the variable for
the reference exporter. The name of the variable depends on the name of the aggregated
variable. If the aggregated variable is "uv", the name of this variable will
be "uv_100_diff".}
}}

\item{var_exporter}{Character indicating the name of the variable containing
the exporters (or the the variable containing entities you want to compare).
Used only if \code{compare = TRUE}. If used, the variable used must alos
be present \code{var_aggregate}. if not used it can be set on NULL.}

\item{exporter_ref}{Character indicating the reference exporter (or entity)
present in the variable given in \code{var_exporter}. Used only if
\code{compare = TRUE}. if not used it can be set on NULL.}

\item{na.rm}{Logical indicating whether NA should be remove from the data for
the computation. By default it is set to TRUE. If FALSE you can obtain NA
if NA are presents in your data be carefull.}

\item{return_output}{Logical indicating whether data must be returned or not.
By default data are returned after this function.}

\item{return_arrow}{Logical indicating whether data must be return in an
arrow format (TRUE the default) or not if \code{return_output = TRUE}.
By default data are returned to arrow format.}

\item{path_output}{Path to save the data. If NULL (default), the data
will not be saved. If \code{path_output} ends with a '.csv' extension, the data
will be saved in csv format. If no extension is given, the data will be
saved in a dataset parquet format in the specified folder. See the
\link[arrow]{arrow} package.}
}
\value{
BACI data with the following variables : variables contained in
\code{var_aggregation}, \code{var}. Depending on the value taken by \code{base_100} and
\code{compare} 2 or 4 more variables can be present. See the explications of these
parameters.
}
\description{
Aggregate a variable in a dataframe using one of the following method :
\itemize{
\item mean
\item median
\item weighted mean
\item weighted median
}

This variable can be transformed in base 100 to allow time comparison.
This base 100 ban also be compared to a reference (by a ratio). The level of
aggregation is freely choosen. Remember that this function (and so the
name and description of parameters) had been think with the idea of beeing
used with a database like BACI and so with variables like "exporter",
"k" (product) or "t" (time).
}
\details{
This feature uses \href{https://arrow.apache.org/docs/r/}{arrow} functionalities.
However, the computation of the various metrics must be in memory
(only this part is in memory). This can take some time depending on your
configuration and the size of your data. If the size of the data is too
large for your computer, it may crash. It is advisable to reduce the size
of your database and run this function several times.
}
\examples{
## Aggregate a variable name "uv" with "mean" at the exporter-year-product
## level. BACI data are used.
## aggregate_compare(
##   baci = "baci-foler-parquet",
##   method = "mean",
##   var = "uv",
##   var_aggregation = c("exporter", "t", "k"),
##   var_temporal = "t",
##   base_100 = FALSE,
##   na.rm = TRUE
## )

## Aggregate a variable name "uv" with "weighted_mean" at the
## region-year-category level with free weight with weight contained in "q".
## BACI data are used. Base 100 is computed and begin in 2015. 
## aggregate_compare(
##   baci = "baci-foler-parquet",
##   method = "weighted_mean",
##   var = "uv",
##   var_aggregation = c("region", "t", "category"),
##   var_temporal = "t",
##   var_weight = "q",
##   fixed_weight = FALSE,
##   base_100 = TRUE,
##   year_ref_base_100 = 2015,
##   compare = FALSE,
##   na.rm = TRUE
## )

## Aggregate a variable name "uv" with "weighted_median" at the
## exporter-year-product level with fixed weight with weight in 2010
## contained in "q". BACI data are used. 
## Base 100 is computed and begin in 2015 and base 100 is compared to France
## aggregate_compare(
##   baci = "baci-foler-parquet",
##   method = "weighted_mean",
##   var = "uv",
##   var_aggregation = c("exporter", "t", "k"),
##   var_temporal = "t",
##   var_weight = "q",
##   fixed_weight = TRUE,
##   year_ref_fixed_weight = 2010,
##   var_disaggregation = c("t", "exporter", "importer", "k"),
##   base_100 = TRUE,
##   year_ref_base_100 = 2015,
##   compare = TRUE,
##   var_exporter = "exporter",
##   exporter_ref = "FRA",
##   na.rm = TRUE
## )

}
\seealso{
\code{\link[=.load_data]{.load_data()}} For more informations concerning the loading.
\code{\link[=.filter_baci]{.filter_baci()}} For more informations concerning the filtering of data inside the function.
\code{\link[=.export_data]{.export_data()}} For more informations concerning the export of the data inside the function.
}
