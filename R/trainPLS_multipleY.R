#' @export
trainPLS_multipleY <- function(x, y, cvsegments = 10, ncomp = "auto", threshold = 0.02){
    n <- ncol(y)
    result <- list()
    for (i in 1:n){
        each_result <- trainPLS(x, y[,i], cvsegments = cvsegments, ncomp = ncomp, threshold = threshold)
        best_index <- which.max(each_result$R2CV)
        result[[i]] <- each_result[best_index,]
    }
    
    result <- do.call(rbind.data.frame, result)
    cbind(data.frame(y = names(y)), result)
}

