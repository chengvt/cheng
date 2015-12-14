#' Train PLS for train dataset by cross-validation
#' 
#' Train PLS for train dataset by cross-validation. The preprocessing method will be optimized automatically.
#' However, the number of latent variables has to be determined manually. Planning to add variable reduction in the future. 
#' 
#' @param x predictor matrix
#' @param y prediction target vector
#' @param maxncomp maximum ncomp for calculation
#' @param cvsegments refer to mvrCv's segments argument
#' @param round round numbers
#' @param reduceVar variable reduction using VIP
#' @param cycles cycles for variable reduction
#' @param fixedncomp fixed numerical value
#' 
#' @import pls
#' @export
trainPLS <- function(x, y, maxncomp = 20, cvsegments = 10, round = 2, reduceVar = FALSE, 
                     cycles = 1, fixedncomp = NULL){
    
    ## set up
    result_list <- list()
    model <- list() 
    
    ## creating a function to select ncomp and return statistical values from the model
    calStats <- function(model){
        plot(model, ncomp = 1:maxncomp, plottype = "validation", type = "b", main = paste("Model", r), cex.lab = 1.3, ylab = "RMSECV", legendpos = "topright") 
        # problem! gotta find a find-knee function for this to work. Let's do manual selection for now
        cat("Model", r, ": ")
        if (is.null(fixedncomp)) {
            ncomp <- as.numeric(readline("Select ncomp: "))
        } else ncomp <- fixedncomp
        localresult <- data.frame(preprocessing = pre,                     
                                  nvar = dim(model$model[[2]])[2],                      
                                  ncomp = ncomp,                                         
                                  R2C = round(getR2(model, ncomp = ncomp, estimate = "train", showprint = FALSE), round),                      
                                  RMSEC = round(getRMSE(model, ncomp = ncomp, estimate = "train", showprint = FALSE), round),                      
                                  R2CV = round(getR2(model, ncomp = ncomp, estimate = "CV", showprint = FALSE), round),                      
                                  RMSECV = round(getRMSE(model, ncomp = ncomp, estimate = "CV", showprint = FALSE), round))
        return(localresult)
    }
    
    ## building models
    # model 1: mean-centering
    r <- 1 # row number
    pre <- "Mean-centering"
    model[[r]] <- plsr(y ~ x, ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
    result_list[[r]] <- calStats(model[[r]])
    
    # model 2: norm + mean-centering
    r <- 2 # row number
    pre <- "Norm + Mean-centering"
    model[[r]] <- plsr(y ~ normalize(x), ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
    result_list[[r]] <- calStats(model[[r]])
    
    # model 3: autoscale
    r <- 3 # row number
    pre <- "Autoscale"
    index <- which(colSums(x) == 0)
    if (length(index) == 0) x_nozero <- x else x_nozero <- x[,-index] # get rid of columns with sum = 0
    model[[r]] <- plsr(y ~ x_nozero, ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments, scale = TRUE)
    result_list[[r]] <- calStats(model[[r]])
    
    ## variable reduction
    if (reduceVar){
        
        if (!exists("VIP")) call_VIP()

        for (cycle in 1:cycles){
            
            # mean-centering
            r <- 1 + (cycle * 3) # row number
            pre <- "Mean-centering"
            VIP_value <- t(VIP(model[[r-3]]))[,result_list[[r-3]]$ncomp]
            index <- which(VIP_value > 1)
            x <- model[[r-3]]$model[[2]]
            x_reduced <- x[,index]
            if (dim(x_reduced)[2] < maxncomp) newncomp <- dim(x_reduced)[2] else newncomp <- maxncomp
            if (dim(x_reduced)[2] == 0) break
            model[[r]] <- plsr(y ~ x_reduced, ncomp = newncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
            result_list[[r]] <- calStats(model[[r]])
            
            # norm + mean-centering
            r <- 2 + (cycle * 3) # row number
            pre <- "Norm + Mean-centering"
            VIP_value <- t(VIP(model[[r-3]]))[,result_list[[r-3]]$ncomp]
            index <- which(VIP_value > 1)
            x <- model[[r-3]]$model[[2]]
            x_reduced <- x[,index]
            if (dim(x_reduced)[2] < maxncomp) newncomp <- dim(x_reduced)[2] else newncomp <- maxncomp
            if (dim(x_reduced)[2] == 0) break
                model[[r]] <- plsr(y ~ x_reduced, ncomp = newncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
            result_list[[r]] <- calStats(model[[r]])
            
            # autoscale
            r <- 3 + (cycle * 3) # row number
            pre <- "Autoscale"
            VIP_value <- t(VIP(model[[r-3]]))[,result_list[[r-3]]$ncomp]
            index <- which(VIP_value > 1)
            x <- model[[r-3]]$model[[2]]
            x_reduced <- x[,index]
            if (dim(x_reduced)[2] < maxncomp) newncomp <- dim(x_reduced)[2] else newncomp <- maxncomp
            if (dim(x_reduced)[2] == 0) break
                model[[r]] <- plsr(y ~ x_reduced, ncomp = newncomp, validation = "CV", method = "oscorespls", segments = cvsegments, scale = TRUE)
            result_list[[r]] <- calStats(model[[r]])
        }        
    }
    result <- do.call(rbind.data.frame, result_list)
    print(result)
    output <- list(result = result, model_list = model)
    return(output)
}