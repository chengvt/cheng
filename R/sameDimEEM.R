#' check that all EEM has the same dimension
#' 
#' check that all EEM has the same dimension
#' 
#' @param EEM a list containing EEM data as created by \code{\link[EEM]{readEEM}} function
#' 
#' @export
sameDimEEM <- function(EEM){
    
    # check that the dimension numbers are the same
    dimMat <- sapply(EEM, dim)
    sameDim <- sum(apply(dimMat, 2, function (x) identical(dimMat[,1], x))) > 0
    
    # check that colnames are the same
    colNames <- sapply(EEM, colnames)
    sameColNames <- sum(apply(colNames, 2, function (x) identical(colNames[,1], x))) > 0
                    
    # check that rownames are the same
    rowNames <- sapply(EEM, rownames)
    sameRowNames <- sum(!apply(rowNames, 2, function (x) identical(rowNames[,1], x))) > 0

    sameDimEEM <- sameDim | sameColNames | sameRowNames
    return(sameDimEEM)
}
