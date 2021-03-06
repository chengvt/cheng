#' Plot SR for EEM data
#' 
#' Plot SR for EEM data
#' 
#' @param x output variable from \code{\link[pls]{plsr}} function
#' @param ncomp number of components/ latent variables
#' @param ... (optional) arguments for \code{\link[EEM]{drawEEM}} and \code{\link[graphics]{filled.contour}} 
#' 
#' @return A figure is returned on the graphic device
#' 
#' @examples
#' require(EEM)
#' data(gluten)
#' gluten_uf <- unfold(gluten) # unfold list into matrix
#' 
#' # delete columns with NA values
#' index <- colSums(is.na(gluten_uf)) == 0
#' gluten_uf <- gluten_uf[, index]
#' gluten_ratio <- as.numeric(names(gluten))
#' 
#' require(pls)
#' model <- plsr(gluten_ratio ~ gluten_uf, ncomp = 3, method = "oscorespls") 
#' plotSR(model) 
#'  
#' @export
#' @import EEM

plotSR <- function(x, ncomp = NULL, ...){
    
    # get SR
    x <- getSR(x)
    
    # get ncomp if not provided
    if (is.null(ncomp)) ncomp = x$ncomp
    
    # plot
    drawEEM(x, ncomp, ...)
    
}
