#' Get SR value from mvr class object
#' 
#' @param x mvr class object
#' @export
getSR <- function(x) {
    
    # extract information
    if (class(x) == "mvr") {
        value <- t(SR(x))
        title <- "SR"
        ncomp <- x$ncomp
    } else {
        stop('Input class not supported.')
    }
    y <- list(title = title, value = value, ncomp = ncomp)
    class(y) <- "EEMweight"
    return(y)
}
