#' Add label to Raman plot
#' 
#' Add label to Raman plot
#' 
#' @param data raman data. accept both matrix and hyperSpec class.
#' @param x x value for labels to appear
#' @param label labels. If not supplied will use sample names in data.
#' 
#' @examples 
#' data(raman)
#' plotRaman(raman)
#' addLabel(raman, 1300)
#' 
#' @export
addLabel <- function(data, x, label = NULL){
    if (class(data) %in% "hyperSpec"){
        if (is.null(label)) label <- data$names
        datamatrix <- data$spc
    } else if (class(data) %in% "matrix"){
        if (is.null(label)) label <- rownames(data)
        datamatrix <- data
    } else break
    all_x <- as.numeric(as.character(colnames(datamatrix)))
    index <- which.min(abs(all_x - x))
    text(colnames(datamatrix)[index], datamatrix[,index], label)

}