#' @export
# Example
# require(EEM)
# data(applejuice)
# country <- sapply(strsplit(names(applejuice), split = "-"), "[", 1)
#
# # ggplot
# drawSpec(unfold(applejuice), EX = 340)
# drawSpec(unfold(applejuice), EX = 340, group = country)
# drawSpec(unfold(applejuice), EM = 400, group = country)
# 
# # base plot
# drawSpec(unfold(applejuice), EX = 340, group = country, ggplot = FALSE)
# drawSpec(unfold(applejuice), EM = 400, group = country, ggplot = FALSE)

drawSpec <- function(EEM_uf, EX = NULL, EM = NULL, group = NULL, ggplot = TRUE,
                     legendlocation = "topright") {
    
    # one of EX of EM must be given
    if (is.null(EX) & is.null(EM)) {
        stop("EX or EM must be specified.")
    }
    
    # EX and EM cannot be both given
    if (!is.null(EX) & !is.null(EM)) {
        stop("Both EX and EM cannot be specified.")
    }
    
    # EX or EM cannot exceed maximum range
    
    # check whether group is given or not
    hasGroup <- !is.null(group)
    
    # if EX is specified
    if (!is.null(EX)){
        idx <- grep(paste0("EX", EX), colnames(EEM_uf))
        xlab <- "Emission wavelength [nm]"
        title <- paste0("EX = ", EX, " nm")
        fixEX = TRUE
    }
    
    # if EM is specified
    if (!is.null(EM)){
        idx <- grep(paste0("EM", EM), colnames(EEM_uf))
        xlab <- "Excitation wavelength [nm]"
        title <- paste0("EM = ", EM, " nm")
        fixEX = FALSE
    }
    
    # select data
    EEM_selected <- EEM_uf[,idx, drop = FALSE]
    
    # if ggplot is enabled, melt the data
    # turn it into data frame
    data <- as.data.frame(EEM_selected, stringsAsFactors = FALSE)
    row.names(data) <- NULL 
    data$sample <- rownames(EEM_selected)
    if (ggplot){
        if (hasGroup){
            data$group <- group
        }
        library(reshape2)
        library(ggplot2)
        data_melted <- melt(data, id.vars = c("sample", if (!is.null(group)) {"group"} else NULL))
        library(dplyr)
        data_melted <- mutate(data_melted, 
                                em = as.numeric(substr(variable, 8, 10)),
                                ex = as.numeric(substr(variable, 3, 5))) 
        
        if (fixEX) {X <- "em"} else {X <- "ex"}
        if (!hasGroup){
            p <- ggplot(data_melted, aes(x = eval(parse(text = X)), y = value, group = sample)) 
        } else {
            p <- ggplot(data_melted, aes(x = eval(parse(text = X)), y = value, group = sample, 
                                         color = group))
        }
        p <- p + 
            geom_line() + theme_bw() + 
            ylab("intensity") + xlab(xlab) +
            theme(text = element_text(size = 18)) + 
            ggtitle(title)
        return(p)
    }

    if (!ggplot){
        data <- t(EEM_selected)
        
        if (fixEX) {
            rownames(data) <- as.numeric(substr(rownames(data), 8, 10))
        } else {
            rownames(data) <- as.numeric(substr(rownames(data), 3, 5))
        }
        if (hasGroup) col <- as.factor(group) else col <- 1
        matplot(rownames(data), data, type = "l", xlab = xlab, ylab = "Intensity",
                main = title, col = col)
        if (hasGroup) legend(legendlocation, legend = levels(as.factor(group)), 
                             col = 1:length(levels(as.factor(group))), lwd = 1)
    }

}


