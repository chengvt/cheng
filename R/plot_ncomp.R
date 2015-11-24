plot_ncomp <- function(model, ncomp = NULL, cex.lab = 1.5, legendpos = "topright", ...){
    rmsep <- RMSEP(model)
    plot(rmsep, type = "b", cex.lab = cex.lab, legendpos = legendpos, ...)
    if (!(is.null(ncomp))) {
        points(ncomp, rmsep$val[1,,ncomp + 1], pch = 19)
        points(ncomp, rmsep$val[2,,ncomp + 1], pch = 17, col = "red")
    }
}
