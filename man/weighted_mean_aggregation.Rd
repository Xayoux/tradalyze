% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_compare.R
\name{weighted_mean_aggregation}
\alias{weighted_mean_aggregation}
\title{Calculate the Weighted Mean of a Variable in a Dataframe.}
\usage{
weighted_mean_aggregation(
  df_baci,
  var,
  var_aggregation,
  fixed_weight,
  var_weight,
  year_ref_fixed_weight = NULL,
  var_disaggregation,
  var_temporal,
  na.rm
)
}
\arguments{
\item{df_baci}{Dataframe.}

\item{var}{Name of the variable to be aggregate by the weighted mean}

\item{var_aggregation}{Character : vector of aggregations variables}

\item{fixed_weight}{Logical indicating if the weight should be kept fixed
or not.}

\item{var_weight}{Name of the variable to be used for the ponderation.}

\item{year_ref_fixed_weight}{Numeric : Year to keep to the fixed weight.}

\item{var_disaggregation}{Vector of character indicating the variables
corresponding to the lower level of aggregation : generally
\code{var_disaggregation = c("exporter", "importer", "k", "t")}.}

\item{var_temporal}{Name of the variable indicating the time.}

\item{na.rm}{Logical indicating whether NA should be removed or not.}
}
\value{
Dataframe of aggregate unit values
}
\description{
Calculate the Weighted Mean of a Variable in a Dataframe.
}
