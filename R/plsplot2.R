#' plot both calibration and validation groups on the same graphs
#' 
#' As the title says
#' 
#' @param model mvr object
#' @export
#' @import pls
plsplot2 <- function(model, newx = NULL, newy = NULL, ncomp = model$ncomp, 
                     location = "bottomright", show = c("ncomp", "R2", "RMSE"),
                     round = 2, fitline = TRUE, cex.pt = 1.5, cex.stats = 1, 
                     xlab = "Measured value", ylab = "Predicted value",
                     col.cal = "black", col.val = "grey", 
                     pch.cal = 21, pch.val = 21, ...){
    
    # get values
    y.cal.m <- model$model[[1]]
    y.cal.p <- predict(model, ncomp = ncomp)
    y.val.m <- newy
    y.val.p <- drop(predict(model, newdata = newx, ncomp = ncomp))
    
    # blank plot
    plot(x = c(y.cal.m, y.val.m), y = c(y.cal.p, y.val.p), type = "n", xlab = xlab, ylab = ylab, ...)
    
    # calibration
    points(y.cal.m, y.cal.p, pch = pch.cal, col = "black", bg = col.cal, cex = cex.pt)

    # validation
    points(y.val.m, y.val.p, pch = pch.val, col = "black", bg = col.val, cex = cex.pt)
    
    # add stats
    addstats(model, ncomp = ncomp, 
             estimate = "test", location = location,
             show = show, round = round, newx = newx, 
             newy = newy, fitline = fitline, cex = cex.stats)
    
    # add legend
    legend("topleft", legend = c("Calibration", "Validation"), 
           pch = c(pch.cal, pch.val), col = "black", pt.bg = c(col.cal, col.val), pt.cex = cex.pt)
    
    }