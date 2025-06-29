% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flow_classification.R
\name{classification_fontagne_1997}
\alias{classification_fontagne_1997}
\title{Perform flow classification from Fontagne et al 1997}
\usage{
classification_fontagne_1997(df_baci, alpha_H, alpha_L, var_weighting, na.rm)
}
\arguments{
\item{df_baci}{BACI dataframe (arrow format)}

\item{alpha_H}{Numeric : Threshold for high variety}

\item{alpha_L}{Numeric : Threshold for low variety}

\item{var_weighting}{Character : name of the variable to perform the ponderation}

\item{na.rm}{Logical : Exclude NA or not}
}
\value{
BACI dataframe with Fontagne et al 1997 classification
}
\description{
Perform flow classification from Fontagne et al 1997
}
