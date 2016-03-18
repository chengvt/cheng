#' Calculate average of EEM
#' 
#' @param EEM_uf unfolded EEM
#' @param use `mean` or `median`
#' 
#' @export
averageEEM <- function(EEM, use = "mean"){
    EEM_uf <- unfold(EEM)
    fun <- switch(use, "mean" = mean, "median" = median)
    average <- apply(EEM_uf, 2, fun)
    average <- fold(average)
    return(average)
}