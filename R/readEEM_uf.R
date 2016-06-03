#' @export

readEEM_uf <- function(file, fold = TRUE){
    EEM <- read.csv(file, row.names = 1)
    if (fold) EEM <- fold(EEM)
    return(EEM)
}