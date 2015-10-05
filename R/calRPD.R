#'@export
#'
calRPD <- function(rmse, y){
    std <- sd(y)
    rpd <- std/rmse
    return(rpd)
}