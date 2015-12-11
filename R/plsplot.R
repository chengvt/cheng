#' Plot prediction graph for mvr model
#' 
#' Plot prediction graph using pls package's `predplot` function but with ability 
#' to add statistical labeling
#' 
#' @param mvr mvr object (pls model)
#' @param ncomp number of component
#' @param location legend position
#' @param show select statistics to show
#' @param round digits to round
#' @param newx newx
#' @param newy newy
#' @param fitline add fitline or not
#' @param cex.stats text size of statistics legend
#' @param ... pass on to predplot
#' 
#' @examples 
#' require(pls)
#' data(yarn)
#' model <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
#' plsplot(model) # calibration set result
#' plsplot(model, estimate = "CV") # cross validation set result
#' 
#' ## customizing the graphs
#' plsplot(model, location = "topleft") # change legend position
#' plsplot(model, show = "R2") # show only R2
#' plsplot(model, show = "R2", round = 4) # round to four digits
#' plsplot(model, fitline = FALSE) # get rid of fitline
#' plsplot(model, show = "R2", cex.stats = 3) # bigger stats font
#' plsplot(model, cex.lab = 1.5, cex.main = 2) # bigger labels font
#' 
#' 
#' @export
#' @import pls
plsplot <- function(mvr, ncomp = mvr$ncomp, 
                    estimate = "train",
                    location = "bottomright",
                    show = c("ncomp", "R2", "RMSE"),
                    round = 2, newx = NULL, newy = NULL,
                    fitline = TRUE, cex.stats = 1, ...){
    if (!(is.null(newx) & is.null(newy))) estimate = "test"
    if (estimate %in% "train"){
        predplot(mvr, ncomp = ncomp, which = estimate, ...)
        addStats(mvr, ncomp = ncomp, 
                 estimate = estimate, location = location,
                 show = show, round = round, newx = newx, 
                 newy = newy, fitline = fitline, cex = cex.stats)
    } else if (estimate %in% "CV"){
        predplot(mvr, ncomp = ncomp, which = "validation", ...)
        addStats(mvr, ncomp = ncomp, 
                 estimate = estimate, location = location,
                 show = show, round = round, newx = newx, 
                 newy = newy, fitline = fitline, cex = cex.stats)
    } else if (estimate %in% "test"){
        predicted <- drop(predict(mvr, ncomp, newdata = newx))
        plot(newy, predicted, ...)
        addStats(mvr, ncomp = ncomp, 
                 estimate = estimate, location = location,
                 show = show, round = round, newx = newx, 
                 newy = newy, fitline = fitline, cex = cex.stats)
    }
    
}