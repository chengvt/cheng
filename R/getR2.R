#' get R2
#' 
#' get R2
#' 
#' @param mvr mvr object
#' @param estimate a subset of c("train", "CV", "test")
#' @param newx predictors for test group
#' @param newy measured value for test group
#' 
#' @export
getR2 <- function(mvr, ncomp = mvr$ncomp, estimate, newx = NULL, newy = NULL){
    if (estimate %in% "test"){
        predicted <- drop(predict(model, ncomp, newdata = newx))
        fit <- lm(predicted ~ newy)
        R2 <- summary(fit)$r.squared
    } else {
        R2 <- R2(mvr, estimate = estimate)$val[1,1,ncomp+1]   
    }
    return(R2)
}