#' @export
getVIP <- function(x) {
    
    # extract information
    if (class(x) == "mvr") {
        value = t(vip(x))
        title = "VIP"
        ncomp = x$ncomp
    } else {
        stop('Input class not supported.')
    }
    y <- list(title = title, value = value, ncomp = ncomp)
    class(y) <- "EEMweight"
    return(y)
}
