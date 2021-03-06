% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trainPLS.R
\name{trainPLS}
\alias{trainPLS}
\title{Train PLS for train dataset by cross-validation}
\usage{
trainPLS(x, y, maxncomp = 20, cvsegments = 10, round = 2,
  reduceVar = FALSE, cycles = 1, ncomp = c("auto", "manual", "fixed"),
  fixedncomp = NULL, threshold = 0.02, saveAllModel = FALSE,
  plotting = TRUE)
}
\arguments{
\item{x}{predictor matrix}

\item{y}{prediction target vector}

\item{maxncomp}{maximum ncomp for calculation}

\item{cvsegments}{refer to mvrCv's segments argument}

\item{round}{round numbers}

\item{reduceVar}{variable reduction using VIP}

\item{cycles}{cycles for variable reduction}

\item{ncomp}{`auto`,`manual` or `fixed`}

\item{fixedncomp}{fixed numerical value}

\item{threshold}{threshold for selecting ncomp}
}
\description{
Train PLS for train dataset by cross-validation. The preprocessing method will be optimized automatically.
However, the number of latent variables has to be determined manually. Planning to add variable reduction in the future.
}
\examples{
library(EEM)
data(gluten)
gluten_uf <- unfold(gluten) # unfold list into matrix

# delete columns with NA values
index <- colSums(is.na(gluten_uf)) == 0
gluten_uf <- gluten_uf[, index]
gluten_ratio <- as.numeric(names(gluten))

result <- trainPLS(gluten_uf, gluten_ratio) 
result

}

