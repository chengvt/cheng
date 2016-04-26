#' Draw multiple contours for EEM data
#' 
#' Draws multiple contours for EEM data using ggplot2 with the help of GridExtra package
#' 
#' @inheritParams drawEEM
#' @inheritParams drawEEMgg
#' @param nrow An integer describing the number of rows in the layout.
#' @param ncol An integer describing the number of columns in the layout.
#' @param ... (optional) arguments for \code{\link{drawEEMggplot}} 
#' 
#' @return Multiple figures returned on the graphic device
#' 
#' @examples
#' require(EEM)
#' data(applejuice)
#' drawMultipleEEM(applejuice[1:5]) # draw contour of the first to the fifth sample
#' 
#' @seealso
#' \code{\link{drawEEM}},\code{\link{drawEEMggplot}}
#' 
#' @export
#' 
#' @import EEM
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
# @importFrom grid viewport

drawMultipleEEM <-
    function(EEM, ggplot = TRUE, textsize = 13, ncol = NULL, nrow = NULL, ...){
        # This function draws multiple EEM using ggplot2
        
        N <- length(EEM)
        
        if (ggplot){
            plot <- list()
            for (i in 1:N){
                plot[[i]] <- drawEEMgg(EEM, i, textsize = textsize, ...)
            }
            
            if (!is.null(ncol)) {
                plot[[N+1]] <- ncol
                names(plot)[N+1] <- "ncol"
            } else if (!is.null(nrow)) {
                plot[[N+1]] <- nrow
                names(plot)[N+1] <- "nrow"
            } else if (!is.null(nrow)&!is.null(col)) {
                plot[[N+1]] <- ncol
                names(plot)[N+1] <- "ncol"
                plot[[N+2]] <- nrow
                names(plot)[N+2] <- "nrow"
            }
            plot[[N+1]] <- do.call(grid.arrange, plot) 
        } else {
            # not possible yet
            # # calculate nrow/ncol
            # if (is.null(nrow) & is.null(ncol)){
            #     ncol <- round(sqrt(N))
            #     nrow <- celing(N/ncol)
            # } else if (is.null(nrow)){
            #     nrow <- celing(N/ncol)
            # } else ncol <- celing(N/nrow)
            # 
            # # create plot positions
            # mat <- matrix(0, nrow = ncol, ncol = nrow) # intentionally
            # mat[1:N] <- 1:N
            # mat <- t(mat)
            # 
            # for (i in 1:N){
            #     col <- which(mat == i, arr.ind = TRUE)[,"col"]
            #     row <- which(mat == i, arr.ind = TRUE)[,"row"]
            #     x_pos <- (col - 1)/ncol
            #     y_pos <- (row - 1)/nrow
            #     vp <- grid::viewport(height = unit(1/nrow, "npc"), 
            #                         width = unit(1/ncol, "npc"), 
            #                         just = c("left","top"), 
            #                         y = y_pos, x = x_pos)
            #     p <- EEM::drawEEM(EEM, i)
            #     print(p, vp = vp)
            # }
            
        }
    }
