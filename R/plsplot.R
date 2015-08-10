#' @export
#' @import pls
plsplot <- function(model, ncomp = length(model$ncomp), 
                    estimate = "train",
                    location = "bottomright",
                    show = c("ncomp", "R2", "RMSE"),
                    round = 2, newx = NULL, newy = NULL,
                    fitline = TRUE, ...){
    if (!(estimate %in% "test")){
        predplot(model, which = estimate, ...)
        addstats(model, ncomp = ncomp, 
                 estimate = estimate, location = location,
                 show = show, round = round, newx = newx, 
                 newy = newy, fitline = fitline)
    } else {
        predicted <- drop(predict(model, ncomp, newdata = newx))
        plot(newy, predicted)
        addstats(model, ncomp = ncomp, 
                 estimate = estimate, location = location,
                 show = show, round = round, newx = newx, 
                 newy = newy, fitline = fitline)
    }
    
}