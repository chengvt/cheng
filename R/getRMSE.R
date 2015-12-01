#' get RMSE from mvr object
#' 
#' Alternative to pls package's `RMSEP`. While `RMSEP` requires a `newdata` dataframe which combines
#' both predictors and target, `getRMSE` lets you put in `newx` and `newy` separately. 
#' Aside from that, declaring `estimate` in `getRMSE` remind you which value you got. 
#' 
#' @param mvr mvr object
#' @param ncomp number of component
#' @param estimate a subset of c("train", "CV", "test")
#' @param newx predictors for test group
#' @param newy measured value for test group
#' @param showprint show result in console
#' 
#' @examples 
#' require(pls)
#' data(yarn)
#' model <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
#' RMSEP(model)
#' getRMSE(model, estimate = "train") # return RMSE at particular ncomp without intercept value
#' getRMSE(model, estimate = "CV")
#' 
#' @export
getRMSE <- function(mvr, ncomp = mvr$ncomp, estimate, newx = NULL, newy = NULL, showprint = TRUE){
    if ((!is.null(newx)) & (!is.null(newy))) estimate <- "test"
    if (estimate %in% "test"){
        predicted <- drop(predict(mvr, ncomp, newdata = newx))
        RMSE <- sqrt(sum((predicted - newy) ^ 2) / length(predicted))
    } else {
        RMSE <- RMSEP(mvr, estimate = estimate)$val[1,1,ncomp+1]
    }
    
    if (showprint){
        output <- "RMSE"
        if (estimate %in% "train") output <- paste0(output, "C")
        if (estimate %in% "CV") output <- paste0(output, "CV")
        if (estimate %in% "test") output <- paste0(output, "P")
        cat(paste0(output, " = ", round(RMSE, 4), " (ncomp = ", ncomp, ")"))
        cat("\n")
    }
    return(RMSE)
}