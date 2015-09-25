#' @export
getVIP <- function(x) {
    
    if (!exists("VIP")) call_VIP()
    
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
