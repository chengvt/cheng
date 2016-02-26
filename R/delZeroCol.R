#' Delete columns filled with zero
#' 
#' Delete columns filled with zero
#' 
#' @param uf unfolded matrix
#' 
#' @examples 
#' require(EEM)
#' data(applejuice)
#' applejuice_uf <- unfold(applejuice)
#' dim(applejuice_uf)
#' applejuice_uf_nozero <- delZeroCol(applejuice_uf)
#' dim(applejuice_uf_nozero)
#' 
#' @export
delZeroCol <- function(uf){
    nonzero_index <- which(!(colSums(abs(uf)) == 0))
    uf_nozero <- uf[, nonzero_index]
    return(uf_nozero)
}