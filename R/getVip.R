#' Get VIP value from mvr class object
#' 
#' @param x mvr class object
#' @export
getVIP <- function(x) {
    
    # extract information
    if (class(x) == "mvr") {
        value = t(VIP(x))
        title = "VIP"
        ncomp = x$ncomp
    } else {
        stop('Input class not supported.')
    }
    y <- list(title = title, value = value, ncomp = ncomp)
    class(y) <- "EEMweight"
    return(y)
}
