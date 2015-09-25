#' Train PLS for train dataset by cross-validation
#' 
#' Train PLS for train dataset by cross-validation. The preprocessing method will be optimized automatically.
#' However, the number of latent variables has to be determined manually. Planning to add variable reduction in the future. 
#' 
#' @param cvsegments refer to mvrCv's segments argument
#' @import pls
#' @export
trainPLS <- function(x, y, newx = NULL, newy = NULL, maxncomp = 20, cvsegments = 10, round = 2){
    
    # matrix output
    result <- matrix(rep(NA, 15), 3, 5)
    rownames(result) <- c("Mean-centering", "Norm + Mean-centering", "Autoscale")
    colnames(result) <- c("ncomp", "R2C", "RMSEC", "R2CV", "RMSECV")
    
    # mean-centering
    r <- 1 # row number
    method <- "Mean-centering"
    model <- plsr(y ~ x, ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
    # problem! gotta find a find-knee function for this to work. Let's do manual selection for now
    plot(model, ncomp = 1:maxncomp, plottype = "validation", type = "b", main = method, cex.lab = 1.3, ylab = "RMSECV") 
    cat(method, ": ")
    ncomp <- as.numeric(readline("Select ncomp: "))
    result[r,1] <- ncomp
    result[r,2] <- round(getR2(model, ncomp = ncomp, estimate = "train"), round)
    result[r,3] <- round(getRMSE(model, ncomp = ncomp, estimate = "train"), round)
    result[r,4] <- round(getR2(model, ncomp = ncomp, estimate = "CV"), round)
    result[r,5] <- round(getRMSE(model, ncomp = ncomp, estimate = "CV"), round)
    
    # norm + mean-centering
    r <- 2 # row number
    method <- "Norm + Mean-centering"
    model <- plsr(y ~ normalize(x), ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
    plot(model, ncomp = 1:maxncomp, plottype = "validation", type = "b", main = method, cex.lab = 1.3, ylab = "RMSECV") 
    cat(method, ": ")
    ncomp <- as.numeric(readline("Select ncomp: "))
    result[r,1] <- ncomp
    result[r,2] <- round(getR2(model, ncomp = ncomp, estimate = "train"), round)
    result[r,3] <- round(getRMSE(model, ncomp = ncomp, estimate = "train"), round)
    result[r,4] <- round(getR2(model, ncomp = ncomp, estimate = "CV"), round)
    result[r,5] <- round(getRMSE(model, ncomp = ncomp, estimate = "CV"), round)
    
    # norm + mean-centering
    r <- 3 # row number
    method <- "Autoscale"
    index <- which(colSums(x) == 0)
    if (length(index) == 0) x_nozero <- x else x_nozero <- x[,-index] # get rid of columns with sum = 0
    model <- plsr(y ~ x_nozero, ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments, scale = TRUE)
    plot(model, ncomp = 1:maxncomp, plottype = "validation", type = "b", main = method, cex.lab = 1.3, ylab = "RMSECV") 
    cat(method, ": ")
    ncomp <- as.numeric(readline("Select ncomp: "))
    result[r,1] <- ncomp
    result[r,2] <- round(getR2(model, ncomp = ncomp, estimate = "train"), round)
    result[r,3] <- round(getRMSE(model, ncomp = ncomp, estimate = "train"), round)
    result[r,4] <- round(getR2(model, ncomp = ncomp, estimate = "CV"), round)
    result[r,5] <- round(getRMSE(model, ncomp = ncomp, estimate = "CV"), round)
    
    print(result)
    return(result)
    
}