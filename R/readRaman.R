#' Read raw Raman files
#' 
#' @param path path to the files or folders which contains raw files
#' 
#' @examples 
#' # not run
#' # data <- readRaman(getwd()) # data in working directory
#' # data <- readRaman("folder") # data in folder
#' # data <- readRaman("file.txt") # specify file
#' # data[1:5,1:5]
#' 
#' @importFrom R.utils isDirectory isFile
#' @export
readRaman <- function(path, hyperSpec = FALSE){
    
    if (nargs() == 0) stop("Folder or file has not been specified.")
    
    # if file names are provided, use them
    if (all(isFile(path))) files <- path else {
        files <- list.files(path, full.names = TRUE, pattern = "\\.txt$")
    }
    
    data <- list()
    sName <- character()
    for (i in 1:length(files)){
        tmpdata <- read.delim(files[i], skip = 3, header = FALSE)
        names(tmpdata) <- c("wavenumber", "intensity")
        data[[i]] <- tmpdata
        sName[i] <- sub("(Name=)(.*)", "\\2", readLines(files[i])[1])
    }
    names(data) <- sName
    
    # assuming that all wavenumbers are the same
    output <- t(do.call(cbind, lapply(data, "[[", "intensity")))
    colnames(output) <- data[[1]]$wavenumber
    
    # turn into hyperSpec object
    if (hyperSpec){
        output <- new("hyperSpec", spc = output, wavelength = colnames(as.numeric(output)), 
            label = list(.wavelength = "1/cm", spc = "counts"), 
            data = data.frame(names = sName, stringsAsFactors = FALSE))
    }
    return(output)
}