#' @export
inflate <- function(x, n){
    if (is.matrix(x)) {
        y <- apply(x, 2, function(x) rep(x, each = n))
    } else if (is.data.frame(x)) {
        y <- as.data.frame(apply(x, 2, function(x) rep(x, each = n)), stringsAsFactors = FALSE)
    } else break
    return(y)
}