% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_ncomp.R
\name{find_ncomp}
\alias{find_ncomp}
\title{Finding ncomp for pls model}
\usage{
find_ncomp(plsmodel_cv, threshold = 0.05)
}
\arguments{
\item{plsmodel_cv}{plsmodel from pls package after performing cross-validation}

\item{threshold}{threshold (see details)}
}
\description{
Automatically determine ncomp from pls model after performing cross-validation
}
\details{
This function extracts RMSECV from pls model and pick the suitable ncomp 
by the following criteria: (1) if the graph is continuously upward, the first component 
will be selected, (2) if the decrease in RMSECV value is less than the specified threshold 
(default threshold = 0.05), the previous ncomp will be selected.
}

