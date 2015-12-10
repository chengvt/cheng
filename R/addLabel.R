#' Add label to Raman plot
#' @export
addLabel <- function(data, x, label = NULL){
    if (class(data) %in% "hyperSpec"){
        if (is.null(label)) label <- hyperSpec$names
        datamatrix <- hyperSpec$spc
    } else if (class(data) %in% "matrix"){
        if (is.null(label)) label <- rownames(data)
        datamatrix <- data
    } else break
    all_x <- as.numeric(as.character(colnames(datamatrix)))
    index <- which.min(abs(all_x - x))
    text(colnames(datamatrix)[index], datamatrix[,index], label)

}