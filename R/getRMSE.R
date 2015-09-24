#' get RMSE
#' 
#' get RMSE
#' 
#' @param mvr mvr object
#' @param estimate a subset of c("train", "CV", "test")
#' @param newx predictors for test group
#' @param newy measured value for test group
#' 
#' @export
getRMSE <- function(mvr, ncomp = mvr$ncomp, estimate, newx = NULL, newy = NULL){
    if (estimate %in% "test"){
        predicted <- drop(predict(model, ncomp, newdata = newx))
        RMSE <- sqrt(sum((predicted - newy) ^ 2) / length(predicted))
    } else {
        RMSE <- RMSEP(mvr, estimate = estimate)$val[1,1,ncomp+1]
    }
    return(RMSE)
}