#' @export
plotLoading_text <- function(PCA, xPC = 1, yPC = 2, labels = NULL){
    loading <- PCA$rotation
    x <- loading[,1]
    y <- loading[,2]
    plot(seq(min(x), max(x), length.out = 3), seq(min(y), max(y), length.out = 3), type = "n")
    text(x,y,labels)
}