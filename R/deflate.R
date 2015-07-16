#' @param x object
#' @param n number of rows per subset to average
#' @param process processing method. Available option: none, average, std
#' @param sdigits significant figures
#' @param FUN function
#' @param ... arguments to pass to function
#' @export

deflate <- function(x, n, ...){
    UseMethod("deflate", x)
}

#' @export
deflate.matrix <- function(x, n, process = "average", sdigits = 2, rowname = NULL){

    # retrieve size of x
    size <- dim(x)
    I <- size[1]
    J <- size[2]
    k <- I/n # number of subsets

    # calculate deflation
    y <- matrix(nrow = k, ncol = J)
    if (process %in% "average") {
        for (i in 1:k) {
            y[i,] <- colMeans(x[(n*(i-1)+1):(n*i),])
        } 
    } else if (process %in% "none"){
        for (i in 1:k) {
            y[i,] <- x[n*(i-1)+1,]
        }
    } else if (process %in% "std"){
        for (i in 1:k) {
            y[i,] <- apply((x[(n*(i-1)+1):(n*i),]), 2, sd)
        }
    } 
    y_round <- signif(y, sdigits)
    
    # transfer names
    colnames(y_round) <- colnames(x)
    rownames(y_round) <- rowname
    return(y_round)

}

#' @export
deflate.character <- function(x, n, FUN, ...){
    
    # retrieve size of x
    I <- length(x)
    k <- I/n # number of subsets
    y <- character(length = k)
    
    for (i in 1:k) {
        y[i] <- x[n*(i-1)+1]
    }
    y <- FUN(y, ...)
    return(y)
}