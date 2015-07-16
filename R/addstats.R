#' Add stats of the model to the plot
#' 
#' Add stats of the model to the plot
#' 
#' @inheritParams pls:::R2
#' @param model object of class `mvr`
#' @param legendlocation location of legend on graph. Look up legend for more details.
#' @export

addstats <- function(model, ncomp = length(model$ncomp), 
                     estimate = "CV",
                     legendlocation = "bottomright",
                     show = c("R2", "RMSECV", "RMSEC")){

    R2 <- round(R2(model, estimate = "train")$val[1,1,ncomp+1], 3)
    leg <- c(as.expression(bquote(ncomp == .(ncomp))),
             as.expression(bquote(italic(R)^2 == .(R2))),
             as.expression(bquote(italic(RMSECV) == .(round(RMSEP(model)$val[1,1,ncomp+1], 2)))),
             as.expression(bquote(italic(RMSEC) == .(round(RMSEP(model, estimate = "train")$val[1,1,ncomp+1], 2))))
             )
    legend(legendlocation, legend = leg, bty = "n")
}