#' Add label to Raman plot
addLabel <- function(hyperSpec, wavelength, label = NULL){
    if (is.null(label)) label <- hyperSpec$names
    datamatrix <- hyperSpec$spc
    all_wavelength <- as.numeric(as.character(colnames(datamatrix)))
    index <- which.min(abs(all_wavelength - wavelength))
    text(colnames(datamatrix)[index], datamatrix[,index], label)
}