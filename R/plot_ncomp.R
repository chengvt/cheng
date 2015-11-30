#' Plot selected ncomp
#' @export
plot_ncomp <- function(model, ncomp = NULL, cex.lab = 1.5, legendpos = "topright", ...){
    rmsep <- RMSEP(model)
    if (nrow(RMSEP(model)$val) == 2) CV <- TRUE else CV <- FALSE
    if (CV) ylab <- "RMSECV" else ylab <- "RMSEC"
    plot(rmsep, type = "b", cex.lab = cex.lab, legendpos = legendpos, ylab = ylab, ...)
    if (!(is.null(ncomp))) {
        points(ncomp, rmsep$val[1,,ncomp + 1], pch = 19)
        if (nrow(RMSEP(model)$val) == 2){
        points(ncomp, rmsep$val[2,,ncomp + 1], pch = 17, col = "red")            
        }
    }
}
