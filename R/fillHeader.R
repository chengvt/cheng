#' Fill in NA with the previous character
#' 
#' @export
fillHeader <- function(x){
    if (is.vector(x)) VAR <- x
    if (is.data.frame(x)) VAR <- names(x)
    for (i in 1:length(VAR)){
        if (is.na(VAR[i])){
            if (i == 1) return
            VAR[i] <- VAR[i-1]
        }
    }
    return(VAR)
}