#' @importFrom car dataEllipse
#' @export
plotScore_ellipse <- function(PCA, x = 1, y = 2, level = 0.95, ...){
    scores <- PCA$x
    MIN <- apply(scores[,c(x,y)], 2, min)
    MAX <- apply(scores[,c(x,y)], 2, max)
    dataEllipse(scores[,x], scores[,y], level = level, xlab = prcompname(PCA, x),
                ylab = prcompname(PCA, y), cex.lab = 1.3, xlim = c(MIN[1], MAX[1]), ylim = c(MIN[2], MAX[2]), ...)
}