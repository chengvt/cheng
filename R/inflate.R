#' @export
inflate <- function(x, n){
    if (is.matrix(x)) {
        y <- apply(x, 2, function(x) rep(x, each = n))
    } else if (is.data.frame(x)) {
        y <- as.data.frame(apply(x, 2, function(x) rep(x, each = n)), stringsAsFactors = FALSE)
        
        # copy class if different
        if (!isTRUE(base::all.equal(sapply(x, class), sapply(y, class)))){
            y <- as.data.frame(mapply(FUN = as, y, sapply(x, class), SIMPLIFY = FALSE))
        }
    } else break
    return(y)
}