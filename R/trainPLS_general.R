#' Train PLS for train dataset by cross-validation
#' 
#' Train PLS for train dataset by cross-validation. The preprocessing method will be optimized automatically.
#' However, the number of latent variables has to be determined manually. Planning to add variable reduction in the future. 
#' 
#' @param x predictor matrix
#' @param y prediction target vector
#' @param maxncomp maximum ncomp for calculation
#' @param cvsegments refer to mvrCv's segments argument
#' @param ncomp `auto`,`manual` or `fixed`
#' @param fixedncomp fixed numerical value
#' @param threshold threshold for selecting ncomp
#' 
#' @import pls
# @import gridGraphics
# @import gridExtra
#' @importFrom grid viewport
# @import EEM
#' 
#' @export
trainPLS_general <- function(x, y, maxncomp = 20, cvsegments = 10,
                           ncomp = c("auto", "manual", "fixed"), fixedncomp = NULL,
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
    layout(matrix(c(1,2,3),3,1), heights = c(1,6,6))
    par(mar = c(0.5, 4.5, 0.5, 0.5))
    frame()
    title_text <- paste0("x: ", deparse(x_varname), " (nvar=", result$nvar[best_model_index], ") y: ", 
                         deparse(y_varname), "\nPreprocessing: ", 
                         result$preprocessing[best_model_index])
    mtext(title_text, side=3, outer=TRUE, line=-3) 
    par(mar = default_mar)
    
    # 1st plot
    plot_ncomp(best_model, ncomp = best_model_ncomp, cex.lab = 1)
    
    # 2nd plot
    plsplot(best_model, ncomp = best_model_ncomp, estimate = "CV", cex.lab = 1)
    
    # reset layout
    layout(matrix(1))
  }
  
  return(output)
}