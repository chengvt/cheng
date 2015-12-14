#' Find columns that are not filled with zero
#' 
#' Find columns that are not filled with zero
#' 
#' @param x matrix
#' 
#' @export
no_zero_col <- function(x){
    return(which(!(colSums(abs(x)) == 0)))
}