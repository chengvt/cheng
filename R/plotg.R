#' @param hyperSpec hyperSpec class object
#' @param group a vector of characters or factors describing categories
#' @param ... extra arguments passing to plotspc
#'
#' @importFrom hyperSpec plotspc
#' @export

plotg <- function(hyperSpec, group, legendlocation = "topright", ...){
    
    # turn group into factor if it isn't already
    if (!is.factor(group)) group <- as.factor(group)
    ngroup <- length(levels(group))
    
    # plot
    plotspc(hyperSpec, col = group, ...)
    legend(legendlocation, col = 1:ngroup, lwd = 2, levels(group))
}

