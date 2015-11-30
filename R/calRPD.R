#'@export
#'
calRPD <- function(rmse, y){
    std <- sd(y)
    rpd <- std/rmse
    return(rpd)
}

#'@export
calRPD2 <- function(model, ncomp, newx, newy){
    rmse <- getRMSE(model, ncomp = ncomp, estimate = "test", newx = newx, newy = newy)
    y <- newy
    rpd <- calRPD(rmse, newy)
    return(rpd)
}