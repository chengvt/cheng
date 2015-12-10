#' Plot PCA loadings for Raman 
#' 
#' Plot PCA loadings for Raman 
#' 
#' @param PCA output variable from \code{\link[stats]{prcomp}}
#' @param ncomp a vector of number of components
#' @param ylab y-axis label
#' @param ... additional arguments for \code{\link[graphic]{matplot}}
#' 
#' @examples 
#' data(raman)
#' PCA <- prcomp(raman)
#' plotRamanLoading(PCA)
#' plotRamanLoading(PCA, ncomp = 1:2) # display only the first two components
#' plotRamanLoading(PCA, ncomp = c(1,3)) # display only the first and the third component
#' 
#' @export
plotRamanLoading <- function(PCA, ncomp = 1:6, xlab = "Raman shift (1/cm)", ylab = "loading", ...){
    par(mar = c(5,4,4,5) + 0.1)
    loading <- PCA$rotation
    wavelength <- rownames(loading)
    matplot(wavelength, loading[, ncomp], lty = 1, type = "l", 
            xlab = xlab, ylab = ylab, ...)
    par(xpd = T)
    legend(par()$usr[2], par()$usr[4], legend = ncomp, lty = 1, col = 1:length(ncomp))
    par(mar = c(5,4,4,2) + 0.1)
    par(xpd = F)
}
