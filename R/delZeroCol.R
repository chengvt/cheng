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
    uf_nozero <- uf[, no_zero_col(uf)]
    return(uf_nozero)
}
