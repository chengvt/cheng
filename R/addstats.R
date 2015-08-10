#' Add stats of the model to the plot
#' 
#' Add stats of the model to the plot
#' 
#' @param model object of class `mvr`
#' @param location location of legend on graph. Look up legend for more details.
#' @param estimate can be "train", "CV" or "test". In case of "test", 
#' newdata must be provided.
#' @param show choose variables to show in the plot
#' @param round number of digits to round
#' @param newx predictors for test group
#' @param newy measured value for test group
#' @inheritParams pls:::R2
#' @export
#' @import pls

addstats <- function(model, ncomp = length(model$ncomp), 
                     estimate = "train",
                     location = "bottomright",
                     show = c("ncomp", "R2", "RMSE"),
                     round = 2, newx = NULL, newy = NULL,
                     fitline = TRUE){
    
    ## get stats
    if (estimate %in% "test"){
        predicted <- drop(predict(model, ncomp, newdata = newx))
        fit <- lm(predicted ~ newy)
        R2 <- summary(fit)$r.squared
        RMSE <- sqrt(sum((predicted - newy) ^ 2) / length(predicted))
        RPD <- sd(newy) / RMSE
    } else {
        R2 <- R2(model, estimate = estimate)$val[1,1,ncomp+1]
        RMSE <- RMSEP(model, estimate = estimate)$val[1,1,ncomp+1]
        measured <- model.response(model.frame(model))
        predicted <- predict(model)
        fit <- lm(predicted ~ measured)
        RPD <- sd(measured) / RMSE
    }
    
    ## arrange stats
    stats <- list()
    for (i in 1:length(show)){
        stats[i] <- get(show[i])
        names(stats)[i] <- show[i]
    }
    
    ## round to significant figure
    stats <- lapply(stats, round, digits = round)

    ## format naming
    # R2
    index <- which((names(stats) %in% "R2"))
    if (estimate %in% "train") {
        names(stats)[index] <- paste0("italic(", names(stats)[index], ")", "*italic(C)") # C for calibration
    } else if (estimate %in% "test") {
        names(stats)[index] <- paste0("italic(", names(stats)[index], ")", "*italic(P)") # P for prediction
    } else if (estimate %in% "CV") {
        names(stats)[index] <- paste0("italic(", names(stats)[index], ")", "*italic(CV)") 
        
    }
    # RMSE
    index <- which((names(stats) %in% "RMSE"))
    if (estimate %in% "train") {
        names(stats)[index] <- paste0("italic(", names(stats)[index], "C)") # C for calibration
    } else if (estimate %in% "test") {
        names(stats)[index] <- paste0("italic(", names(stats)[index], "P)") # P for prediction
    } else if (estimate %in% "CV") {
        names(stats)[index] <- paste0("italic(", names(stats)[index], "CV)") 
    }
    
    ## superscript 2
    names(stats) <- sub("R2)", "R)^2", names(stats))
    
    ## add legend
    leg <- c(parse(text = paste(bquote(.(names(stats)[1])), "==", bquote(.(stats[[1]])))),
             if (length(stats) > 1) parse(text = paste(bquote(.(names(stats)[2])), "==", bquote(.(stats[[2]])))),
             if (length(stats) > 2) parse(text = paste(bquote(.(names(stats)[3])), "==", bquote(.(stats[[3]])))))
         
    legend(location, legend = leg, bty = "n")
    
    ## add fit line
    if (fitline) abline(a = coef(fit)[1], b = coef(fit)[2], lty = "dashed")
}