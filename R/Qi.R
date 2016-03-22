#' @export
Qi <- function(prcomp, X, i = 1, k){
    # assume that no scaling was used for now
    Xcen <- X - prcomp$center
    Xceni <- Xcen[i,,drop = FALSE]
    Pk <- prcomp$rotation[,1:k]
    I <- diag(nrow(Pk))
    Qi <- Xceni %*% (I - (Pk %*% t(Pk))) %*% t(Xceni)
    return(Qi)
}