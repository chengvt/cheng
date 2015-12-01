#' get R2 from mvr object
#' 
#' Alternative to pls package's `R2`. While `R2` requires a `newdata` dataframe which combines
#' both predictors and target, `getR2` lets you put in `newx` and `newy` separately. 
#' Aside from that, declaring `estimate` in `getR2` remind you which value you got. 
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
#' R2(model)
#' getR2(model, estimate = "train") # return R2 at particular ncomp without intercept value
#' getR2(model, estimate = "CV")
#' 
#' @export
getR2 <- function(mvr, ncomp = mvr$ncomp, estimate, newx = NULL, newy = NULL, showprint = TRUE){
    if (estimate %in% "test"){
        predicted <- drop(predict(mvr, ncomp, newdata = newx))
        fit <- lm(predicted ~ newy)
        R2 <- summary(fit)$r.squared
    } else {
        R2 <- R2(mvr, estimate = estimate)$val[1,1,ncomp+1]   
    }
    
    if (showprint){
        output <- "R2"
        if (estimate %in% "train") output <- paste0(output, "C")
        if (estimate %in% "CV") output <- paste0(output, "CV")
        if (estimate %in% "test") output <- paste0(output, "P")
        cat(paste0(output, " = ", round(R2, 4), " (ncomp = ", ncomp, ")"))
        cat("\n")
    }
    return(R2)
}