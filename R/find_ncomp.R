#' Finding ncomp for pls model
#' 
#' Automatically determine ncomp from pls model after performing cross-validation
#' 
#' @param plsmodel_cv plsmodel from pls package after performing cross-validation
#' @param threshold threshold (see details)
#' @details This function extracts RMSECV from pls model and pick the suitable ncomp 
#' by the following criteria: (1) if the graph is continuously upward, the first component 
#' will be selected, (2) if the decrease in RMSECV value is less than the specified threshold 
#' (default threshold = 0.05), the previous ncomp will be selected.
#' 
#' @importFrom pls RMSEP
#' @export
find_ncomp <- function(plsmodel_cv, threshold = 0.05){
    
    # shorten variable name 
    model <- plsmodel_cv
    
    # get RMSECV 
    RMSECV <- pls::RMSEP(model)$val[1,1,-1] # without intercept
    n <- length(RMSECV)
    
    # (1) if all other RMSECV is higher that the first one, use ncomp = 1
    if (which.min(RMSECV) == 1) {
        ncomp <- 1
    } else {
        # (2) if the change in RMSECV becomes lesser than 2% use the previous ncomp
        i <- 2
        change <- (RMSECV[i-1] - RMSECV[i]) / (RMSECV[1] - RMSECV[i])
        while (change > threshold){
            i <- i + 1
            if (i > n) break
            change <- (RMSECV[i-1] - RMSECV[i]) / (RMSECV[1] - RMSECV[i])
        }
        ncomp <- i - 1
    }
    
    ncomp
    
}