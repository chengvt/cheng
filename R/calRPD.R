#' Calculate RPD score
#' 
#' Calculate RPD score
#' 
#'@export
#'
calRPD <- function(rmse, y){
    std <- sd(y)
    rpd <- std/rmse
    return(rpd)
}

#'@export
calRPD2 <- function(mvr, ncomp, newx, newy){
    rmse <- getRMSE(mvr, ncomp = ncomp, estimate = "test", newx = newx, newy = newy, showprint = FALSE)
    y <- newy
    rpd <- calRPD(rmse, newy)
    return(rpd)
}