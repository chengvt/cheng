#' @export
Q <- function(prcomp, X, k){
    # assume that no scaling was used for now
    Xcen <- X - prcomp$center
    Pk <- prcomp$rotation[,1:k]
    I <- diag(nrow(Pk))
    Qi <- function(Xi){
        Xi <- matrix(Xi, 1, length(Xi)) 
        Qi <- Xi %*% (I - (Pk %*% t(Pk))) %*% t(Xi)
        return(Qi)
    }
    
    Q <- apply(Xcen, 1, Qi)
    return(Q)
    # seems like the calculation is still wrong
}