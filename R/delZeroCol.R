#' @export
delZeroCol <- function(uf){
    uf_nozero <- uf[, no_zero_col(uf)]
    return(uf_nozero)
}
