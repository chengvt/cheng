no_zero_col <- function(x){
    return(which(!(colSums(abs(x)) == 0)))
}