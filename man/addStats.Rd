% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addstats.R
\name{addStats}
\alias{addStats}
\title{Add stats of the model to the plot}
\usage{
addStats(model, ncomp = model$ncomp, estimate = "train",
  location = "bottomright", show = c("ncomp", "R2", "RMSE"), round = 2,
  newx = NULL, newy = NULL, fitline = TRUE, ...)
}
\arguments{
\item{model}{object of class `mvr`}

\item{ncomp}{a vector of positive integers.  The components or number
    of components to use.  See below.}

\item{estimate}{can be "train", "CV" or "test". In case of "test", 
newdata must be provided.}

\item{location}{location of legend on graph. Look up legend for more details.}

\item{show}{choose variables to show in the plot}

\item{round}{number of digits to round}

\item{newx}{predictors for test group}

\item{newy}{measured value for test group}

\item{fitline}{logical argument whether to add fitline or not}

\item{...}{additional arguments for \code{\link{legend}}}
}
\description{
Add stats of the model to the plot
}

