#' @export
T2 <- function(prcomp, k){
    scores <- prcomp$x
    lambda <- diag(prcomp$sdev[1:k]^2)
    
    T2 <- vector() #initialize
    for (i in 1:nrow(scores)){
        Ti <- scores[i,1:k, drop = FALSE]
        Ti2 <- Ti %*% lambda %*% t(Ti)
        T2[i] <- Ti2 
    }
    return(T2)
}