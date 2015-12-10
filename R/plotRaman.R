#' Plot raman spectra
#' 
#' Plot raman spectra
#' 
#' @param data matrix or hyperSpec class object
#' @param group a vector of characters or factors describing categories
#' @param legendlocation location of legend. can be anything from "bottomright", 
#' "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" or "center".
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param ... extra arguments passing to plotspc or matplot
#'
#' @import hyperSpec
#' @export

plotRaman <- function(data, group = NULL, legendlocation = "topright", 
                      xlab = "Raman shift (1/cm)", ylab = "Counts", ...){
    
    # check if group information is provided
    has_g <- !is.null(group)
    
    if (has_g){
        # turn group into factor if it isn't already
        if (!is.factor(group)) group <- as.factor(group)
        ngroup <- length(levels(group))
    }
    
    # if data is data.frame turn it to matrix
    if (is.data.frame(data)) data <- as.matrix(data)

    if (class(data) %in% "hyperSpec"){
        if (has_g){
            plotspc(hyperSpec, col = group, title.args = list(xlab = xlab, y = lab), ...)
            legend(legendlocation, col = 1:ngroup, lwd = 2, levels(group))
        } else  plotspc(hyperSpec, title.args = list(xlab = xlab, y = lab), ...)
    } else if (class(data) %in% "matrix") {
        if (has_g){
            matplot(colnames(data), t(data), type = "l", lty = 1, xlab = xlab, ylab = ylab, col = group)
            legend("topright", col = 1:ngroup, lwd = 1, legend = levels(group), ...)
        } else {
            matplot(colnames(data), t(data), type = "l", lty = 1, xlab = xlab, ylab = ylab, ...)
        }
    }
}

