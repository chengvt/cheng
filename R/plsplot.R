#' @export
#' @import pls
plsplot <- function(model, ncomp = model$ncomp, 
                    estimate = "train",
                    location = "bottomright",
                    show = c("ncomp", "R2", "RMSE"),
                    round = 2, newx = NULL, newy = NULL,
                    fitline = TRUE, ...){
    if (!(is.null(newx) & is.null(newy))) estimate = "test"
    if (estimate %in% "train"){
        predplot(model, which = estimate, ...)
        addstats(model, ncomp = ncomp, 
                 estimate = estimate, location = location,
                 show = show, round = round, newx = newx, 
                 newy = newy, fitline = fitline)
    } else if (estimate %in% "CV"){
        predplot(model, which = "validation", ...)
        addstats(model, ncomp = ncomp, 
                 estimate = estimate, location = location,
                 show = show, round = round, newx = newx, 
                 newy = newy, fitline = fitline)
    } else if (estimate %in% "test"){
        predicted <- drop(predict(model, ncomp, newdata = newx))
        plot(newy, predicted, xlab = "measured", ...)
        addstats(model, ncomp = ncomp, 
                 estimate = estimate, location = location,
                 show = show, round = round, newx = newx, 
                 newy = newy, fitline = fitline)
    }
    
}