#' Plot raman spectra
#' 
#' Plot raman spectra
#' 
#' @param hyperSpec hyperSpec class object
#' @param group a vector of characters or factors describing categories
#' @param ... extra arguments passing to matplot
#'
#' @importFrom hyperSpec plotspc
#' @export

plotRaman <- function(data, group = NULL, legendlocation = "topright", ...){
    
    # check if group information is provided
    has_g <- !is.null(group)
    
    if (has_g){
        # turn group into factor if it isn't already
        if (!is.factor(group)) group <- as.factor(group)
        ngroup <- length(levels(group))
    }

    if (class(data) %in% "hyperSpec"){
        if (has_g){
            plotspc(hyperSpec, col = group)
            legend(legendlocation, col = 1:ngroup, lwd = 2, levels(group))
        } else  plotspc(hyperSpec)
    } else if (class(data) %in% "matrix") {
        if (has_g){
            matplot(colnames(data), t(data), type = "l", lty = 1, xlab = "wavelength", ylab = "Intensity", col = group)
            legend("topright", col = 1:ngroup, lwd = 1, legend = levels(group))
        } else {
            matplot(colnames(data), t(data), type = "l", lty = 1, xlab = "wavelength", ylab = "Intensity")
        }
    }
}

