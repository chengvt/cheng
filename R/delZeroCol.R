#' @export
delZeroCol <- function(uf){
    index <- which(colSums(uf) == 0)
    uf_nozero <- uf[, -index]
    return(uf_nozero)
}
