#' @export
inflate <- function(x, n){
    if (is.matrix(x)) {
        y <- apply(x, 2, function(x) rep(x, each = n))
    } else if (is.data.frame(x)) {
        y <- apply(x, 2, function(x) rep(x, each = n))
    } else break
    return(y)
}