% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_uv_outliers.R
\name{method_sd}
\alias{method_sd}
\title{Method sd for Detecting Outliers}
\usage{
method_sd(df_baci, alpha_H, alpha_L, na.rm, rm_temp_var)
}
\arguments{
\item{df_baci}{BACI dataframe}

\item{alpha_H}{Threshold for high outliers}

\item{alpha_L}{Threshold for low outliers}

\item{na.rm}{Exclude NA or not}

\item{rm_temp_var}{Remove temporary variables or not}
}
\value{
BACI df with outliers Highlighted (R dataframe format)
}
\description{
Method sd for Detecting Outliers
}
