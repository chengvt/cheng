#' Plot pls results for both calibration and validation groups
#' 
#' Plot pls results for both calibration and validation groups on the same graph.
#' 
#' @param mvr mvr object
#' @param ncomp number of component
#' @param location legend position
#' @param show select statistics to show
#' @param round digits to round
#' @param newx newx
#' @param newy newy
#' @param fitline add fitline or not
#' @param cex.stats text size of statistics legend
#' @param cex.pt point size
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param col.cal point color for calibration group
#' @param col.val point color for validation group
#' @param pch.cal pch for calibration group
#' @param pch.val pch for validation group
#' @param ... pass on to plot
#' 
#' @examples 
#' 
#' require(pls)
#' data(yarn)
#' yarn.cal <- yarn[yarn$train,]
#' yarn.val <- yarn[!yarn$train,]
#' model <- plsr(density ~ NIR, 15, data = yarn.cal, validation = "CV")
#' plot_ncomp(model)
#' ncomp <- 4 # 4 components seem to be appropriate
#' model <- plsr(density ~ NIR, ncomp, data = yarn.cal) # recalculate
#' 
#' plsplot(model) # calibration
#' plsplot(model, newx = yarn.val$NIR, newy = yarn.val$density) # validation
#' 
#' # now put those two plots together
#' plsplot2(model, newx = yarn.val$NIR, newy = yarn.val$density) # calibration and validation
#' plsplot2(model, newx = yarn.val$NIR, newy = yarn.val$density, col.cal = "forestgreen", col.val = "skyblue") # change point colors
#' 
#' @export
#' @import pls
plsplot2 <- function(mvr, newx = NULL, newy = NULL, ncomp = mvr$ncomp, 
                     location = "bottomright", show = c("ncomp", "R2", "RMSE"),
                     round = 2, fitline = TRUE, cex.pt = 1.5, cex.stats = 1, 
                     xlab = "Measured value", ylab = "Predicted value",
                     col.cal = "black", col.val = "grey", 
                     pch.cal = 21, pch.val = 21, ...){
    
    # get values
    y.cal.m <- mvr$model[[1]]
    y.cal.p <- predict(mvr, ncomp = ncomp)
    y.val.m <- newy
    y.val.p <- drop(predict(mvr, newdata = newx, ncomp = ncomp))
    
    # blank plot
    plot(x = c(y.cal.m, y.val.m), y = c(y.cal.p, y.val.p), type = "n", xlab = xlab, ylab = ylab, ...)
    
    # calibration
    points(y.cal.m, y.cal.p, pch = pch.cal, col = "black", bg = col.cal, cex = cex.pt)
    
    # validation
    points(y.val.m, y.val.p, pch = pch.val, col = "black", bg = col.val, cex = cex.pt)
    
    # add stats
    addStats(mvr, ncomp = ncomp, 
             estimate = "test", location = location,
             show = show, round = round, newx = newx, 
             newy = newy, fitline = fitline, cex = cex.stats)
    
    # add legend
    legend("topleft", legend = c("Calibration", "Validation"), 
           pch = c(pch.cal, pch.val), col = "black", pt.bg = c(col.cal, col.val), pt.cex = cex.pt)
    
}