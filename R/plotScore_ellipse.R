#' Plot PCA scores with ellipse 
#' 
#' Plot PCA scores with ellipse using \code{\link{dataEllipse}}
#' 
#' @examples 
#' require(EEM)
#' data(applejuice)
#' applejuice_uf <- unfold(applejuice)
#' PCA <- prcomp(applejuice_uf)
#' plotScore_ellipse(PCA)
#' 
#' # change level
#' plotScore_ellipse(PCA, level = 0.8) 
#'
#' # manually set x,y ranges
#' plotScore_ellipse(PCA, xlim = c(-9000, 9000), ylim = c(-4000, 4000)) 
#' 
#' # fill in circles
#' plotScore_ellipse(PCA, fill = TRUE, 
#' fill.alpha = 0.2, xlim = c(-9000, 9000), ylim = c(-4000, 4000))
#' 
#' 
#' @importFrom car dataEllipse
#' @export
plotScore_ellipse <- function(PCA, x = 1, y = 2, level = 0.95, xlim = NULL, ylim = NULL, ...){
    scores <- PCA$x
    if (is.null(xlim)) xlim <- c(min(scores[, x]), max(scores[, x]))  
    if (is.null(ylim)) ylim <- c(min(scores[, y]), max(scores[, y])) 
    
    dataEllipse(scores[, x], scores[, y], level = level, xlab = prcompname(PCA, x),
                ylab = prcompname(PCA, y), cex.lab = 1.3, xlim = xlim, 
                ylim = ylim, ...)
}