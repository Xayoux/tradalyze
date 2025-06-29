% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_uv_outliers.R
\name{method_be11}
\alias{method_be11}
\title{Method be11 for Detecting Outliers}
\usage{
method_be11(df_baci, alpha_H, alpha_L, beta, na.rm, rm_temp_var)
}
\arguments{
\item{df_baci}{BACI dataframe}

\item{alpha_H}{Threshold for high outliers}

\item{alpha_L}{Threshold for low outliers}

\item{beta}{Threshold for temporal outliers}

\item{na.rm}{Exclude NA or not}

\item{rm_temp_var}{Remove temporary variables or not}
}
\value{
BACI df with outliers Highlighted (R dataframe format)
}
\description{
Method be11 for Detecting Outliers
}
