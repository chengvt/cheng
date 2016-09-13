#' predict function for trainPLS object
#' 
#' predict function for trainPLS object
#' 
#' @param x trainPLS object
#' @param ... arguments for \code{predict} function
#' 
#' @export
predict.trainPLS <- function(object, newx, ...){
  
  # preprocess newx
  x <- as.matrix(newx)
  if (object$bestmodel_pre == "Norm + Mean-centering") {
    x <- normalize(x)
  } else if (object$bestmodel_pre == "Autoscale") {
    index <- which(colSums(x) == 0)
    if (length(index) == 0) x <- x else x <- x[,-index]
  }
  
  # predict newy
  newy <- predict(object$bestmodel, ncomp = object$bestmodel_ncomp)[,1,1]
  
  return(newy)
  
}