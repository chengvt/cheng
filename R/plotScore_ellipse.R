#' @importFrom car dataEllipse
#' @export
plotScore_ellipse <- function(PCA, x = 1, y = 2, level = 0.95, xlim = NULL, ylim = NULL, ...){
    scores <- PCA$x
    if (is.null(xlim)) xlim <- c(min(scores[,x]), max(scores[,x]))  
    if (is.null(ylim)) ylim <- c(min(scores[,y]), max(scores[,y])) 
    
    dataEllipse(scores[,x], scores[,y], level = level, xlab = prcompname(PCA, x),
                ylab = prcompname(PCA, y), cex.lab = 1.3, xlim = xlim, 
                ylim = ylim, ...)
}