#' @export
plotScore_ellipse <- function(PCA, x = 1, y = 2){
    scores <- PCA$x
    ellipse_matrix <- dataEllipse(scores[,x], scores[,y], level = .997, xlab = prcompname(PCA, x),
                                  ylab = prcompname(PCA, y), cex = 1.3, draw = F)
    MIN <- apply(ellipse_matrix, 2, min)
    MAX <- apply(ellipse_matrix, 2, max)
    dataEllipse(scores[,x], scores[,y], level = .997, xlab = prcompname(PCA, x),
                ylab = prcompname(PCA, y), cex = 1.3, xlim = c(MIN[1], MAX[1]), ylim = c(MIN[2], MAX[2]))
}