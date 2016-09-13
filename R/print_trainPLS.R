#' Print trainPLS
#' 
#' Print trainPLS
#' 
#' @param x trainPLS object
#' @param ... arguments for \code{print} function
#' 
#' @export
print.trainPLS <- function(x, ...) {
  print(x$result)
}