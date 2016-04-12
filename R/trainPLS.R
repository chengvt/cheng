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
#' @param ncomp `auto`,`manual` or `fixed`
#' @param fixedncomp fixed numerical value
#' @param threshold threshold for selecting ncomp
#' 
#' @import pls
#' @import gridGraphics
#' @import gridExtra
#' @import grid
#' @export
trainPLS <- function(x, y, maxncomp = 20, cvsegments = 10, round = 2, reduceVar = FALSE, 
                     cycles = 1, ncomp = c("auto", "manual", "fixed"), fixedncomp = NULL,
                     threshold = 0.02, saveModel = FALSE, plotting = TRUE){

    ## set up
    x_varname <-  substitute(x)
    y_varname <-  substitute(y)
    result_list <- list()
    model <- list() 
    if (length(ncomp) == 3) ncomp <- "auto"
    if (!is.matrix(x)) x <- as.matrix(x)
    if (!is.matrix(y)) y <- as.matrix(y)
    if (maxncomp > nrow(x)) maxncomp <- nrow(x) - 1

    ## creating a function to select ncomp and return statistical values from the model
    calStats <- function(model){
        
        ## selecting ncomp depending on each problem.
        if (ncomp == "auto"){
            ncomp <- find_ncomp2(model, threshold = threshold)
        } else if (ncomp == "fixed"){
            if (is.null(fixedncomp)) break
            ncomp <- fixedncomp
        } else {
            plot(model, ncomp = 1:maxncomp, plottype = "validation", type = "b", main = paste("Model", r), cex.lab = 1.3, ylab = "RMSECV", legendpos = "topright") 
            cat("Model", r, ": ")
            ncomp <- as.numeric(readline("Select ncomp: "))
        }
        
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
    if (saveModel) output <- list(result = result, model_list = model) else {
        output <- result
    }
    
    # plot
    if (plotting){

        # find best model
        best_model_index <- which.min(result$RMSECV)
        best_model <- model[[best_model_index]]
        best_model_ncomp <- result$ncomp[best_model_index]
        
        # plot layout
        default_mar <- c(5, 4, 4, 2) + 0.1
        layout(matrix(c(1,2,4,1,3,5),3,2), heights = c(1,6,6))
        par(mar = c(0.5, 4.5, 0.5, 0.5))
        frame()
        title_text <- paste0("x: ",x_varname, " y: ", y_varname, "\nPreprocessing: ", 
                             result$preprocessing[best_model_index])
        mtext(title_text, side=3, outer=TRUE, line=-3) 
        par(mar = default_mar)
        
        # 1st plot
        plot_ncomp(best_model, ncomp = best_model_ncomp, cex.lab = 1)
        
        # 2nd plot
        plsplot(best_model, ncomp = best_model_ncomp, estimate = "CV", cex.lab = 1)
        
        # 3rd plot
        vp.BottomLeft <- grid::viewport(height=unit(0.4, "npc"), width=unit(0.5, "npc"), 
                                  just=c("left","top"), y=0.45, x=0)
        p_VIP <- drawEEMgg(getVIP(best_model), ncomp = best_model_ncomp, textsize = 12)
        print(p_VIP,vp = vp.BottomLeft)
        
        # 4th plot
        vp.BottomRight <- grid::viewport(height=unit(0.4, "npc"), width=unit(0.5, "npc"), 
                                   just=c("left","top"), 
                                   y=0.45, x=0.5)
        p_Reg <- drawEEMgg(getReg(best_model), ncomp = best_model_ncomp, textsize = 12)
        print(p_Reg,vp = vp.BottomRight)
    }
    
    return(output)
}