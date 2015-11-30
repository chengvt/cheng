#' Train PLS for train dataset by cross-validation
#' 
#' Train PLS for train dataset by cross-validation. The preprocessing method will be optimized automatically.
#' However, the number of latent variables has to be determined manually. This is different from trainPLS as 
#' you have to specify the preprocessing method manually.
#' 
#' @param cvsegments refer to mvrCv's segments argument
#' @param fixedncomp fixed numerical value
#' @import pls
#' @export
trainPLS2 <- function(x, y, newx = NULL, newy = NULL, maxncomp = 20, cvsegments = 10, round = 2, reduceVar = TRUE, 
                      cycles = 3, fixedncomp = NULL, prepro = c("mc", "norm_mc","au")){
    
    ## set up
    result_list <- list()
    model <- list() 
    if (length(prepro) == 3) prepro <- "mc"
    if ((!is.null(newx)) & (!is.null(newy))) newdata <- TRUE else newdata <- FALSE
    
    ## creating a function to select ncomp and return statistical values from the model
    calStats <- function(model, newx, newy){
        plot(model, ncomp = 1:maxncomp, plottype = "validation", type = "b", main = paste("Model", r), cex.lab = 1.3, ylab = "RMSECV", legendpos = "topright") 
        # problem! gotta find a find-knee function for this to work. Let's do manual selection for now
        cat("Model", r, ": ")
        if (is.null(fixedncomp)) {
            ncomp <- as.numeric(readline("Select ncomp: "))
        } else ncomp <- fixedncomp
        localresult <- data.frame(preprocessing = pre,                     
                                  nvar = dim(model$model[[2]])[2],                      
                                  ncomp = ncomp,                                         
                                  R2C = round(getR2(model, ncomp = ncomp, estimate = "train"), round),                      
                                  RMSEC = round(getRMSE(model, ncomp = ncomp, estimate = "train"), round),                      
                                  R2CV = round(getR2(model, ncomp = ncomp, estimate = "CV"), round),                      
                                  RMSECV = round(getRMSE(model, ncomp = ncomp, estimate = "CV"), round))
        if (newdata){
            localresult_p <- data.frame(R2P = round(getR2(model, ncomp = ncomp, newx = newx, newy = newy, estimate = "test"), round),
                                        RMSEP = round(getRMSE(model, ncomp = ncomp, newx = newx, newy = newy, estimate = "test"), round),
                                        RPD = round(calRPD2(model, ncomp = ncomp, newx = newx, newy = newy), round))
            localresult <- cbind(localresult, localresult_p)
        }
        
        return(localresult)
    }
    
    ## building models
    if (prepro %in% "mc"){
        # model 1: mean-centering
        r <- 1 # row number
        pre <- "Mean-centering"
        model[[r]] <- plsr(y ~ x, ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
        result_list[[r]] <- calStats(model[[r]])
        
        if (reduceVar){
            
            if (!exists("VIP")) call_VIP()
            
            for (cycle in 1:cycles){
                
                # mean-centering
                r <- 1 + cycle  # row number
                pre <- "Mean-centering"
                VIP_value <- t(VIP(model[[r-1]]))[,result_list[[r-1]]$ncomp]
                index <- which(VIP_value > 1)
                x <- model[[r-1]]$model[[2]]
                x_reduced <- x[,index, drop = FALSE]
                if (dim(x_reduced)[2] < maxncomp) newncomp <- dim(x_reduced)[2] else newncomp <- maxncomp
                if (dim(x_reduced)[2] == 0) stop(paste0("The number of variables reaches zero after ", cycle, " cycles."))
                model[[r]] <- plsr(y ~ x_reduced, ncomp = newncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
                result_list[[r]] <- calStats(model[[r]], newx, newy)

            }        
        }
        
    } else if (prepro %in% "norm_mc"){
        # model 2: norm + mean-centering
        r <- 1 # row number
        pre <- "Norm + Mean-centering"
        model[[r]] <- plsr(y ~ normalize(x), ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
        result_list[[r]] <- calStats(model[[r]])
        
        if (reduceVar){
            
            if (!exists("VIP")) call_VIP()
            
            for (cycle in 1:cycles){
                
                # norm + mean-centering
                r <- 1 + cycle  # row number
                pre <- "Norm + Mean-centering"
                VIP_value <- t(VIP(model[[r-1]]))[,result_list[[r-1]]$ncomp]
                index <- which(VIP_value > 1)
                x <- model[[r-1]]$model[[2]]
                x_reduced <- x[,index]
                if (dim(x_reduced)[2] < maxncomp) newncomp <- dim(x_reduced)[2] else newncomp <- maxncomp
                if (dim(x_reduced)[2] == 0) stop(paste0("The number of variables reaches zero after ", cycle, " cycles."))
                model[[r]] <- plsr(y ~ x_reduced, ncomp = newncomp, validation = "CV", method = "oscorespls", segments = cvsegments)
                result_list[[r]] <- calStats(model[[r]], newx, newy)
                
            }        
        }
    } else if (prepro %in% "au"){
        # model 3: autoscale
        r <- 1 # row number
        pre <- "Autoscale"
        index <- which(colSums(x) == 0)
        if (length(index) == 0) x_nozero <- x else x_nozero <- x[,-index] # get rid of columns with sum = 0
        model[[r]] <- plsr(y ~ x_nozero, ncomp = maxncomp, validation = "CV", method = "oscorespls", segments = cvsegments, scale = TRUE)
        result_list[[r]] <- calStats(model[[r]])
        
        if (reduceVar){
            
            if (!exists("VIP")) call_VIP()
            
            for (cycle in 1:cycles){
                
                # autoscale
                r <- 1 + cycle # row number
                pre <- "Autoscale"
                VIP_value <- t(VIP(model[[r-1]]))[,result_list[[r-1]]$ncomp]
                index <- which(VIP_value > 1)
                x <- model[[r-1]]$model[[2]]
                x_reduced <- x[,index]
                if (dim(x_reduced)[2] < maxncomp) newncomp <- dim(x_reduced)[2] else newncomp <- maxncomp
                if (dim(x_reduced)[2] == 0) stop(paste0("The number of variables reaches zero after ", cycle, " cycles."))
                model[[r]] <- plsr(y ~ x_reduced, ncomp = newncomp, validation = "CV", method = "oscorespls", segments = cvsegments, scale = TRUE)
                result_list[[r]] <- calStats(model[[r]], newx, newy)
            }        
        }
    }
    
    ## variable reduction
    
    result <- do.call(rbind.data.frame, result_list)
    print(result)
    output <- list(result = result, model_list = model)
    return(output)
}