#' Draw contour for EEM data using ggplot2
#' 
#' This function draw contour for EEM data using ggplot2.
#' 
#' @param textsize (optional) text size
#' @param geom (optional) object to display data. Can be "polygon" or "path". 
#' 
#' @return A figure is returned on the graphic device
#' 
#' @details \code{\link{drawEEM}} is faster and should be used. But since it lacks multiple plotting capacity, \code{\link{drawEEMgg}} was created to work with \code{\link{drawMultipleEEM}}.
#' 
#' @examples
#' data(AppleJuice)
#' drawEEMgg(AppleJuice, n = 36) # draw EEM of sample no.36
#' drawEEMgg(AppleJuice, n = 36, color = cm.colors) # draw EEM of sample no.36 with different color
#' drawEEMgg(AppleJuice, n = 36, geom = "path", nlevel = 20) # draw EEM of sample no.36 with different color
#' 
#' @seealso
#' \code{\link{drawEEM}},\code{\link{drawMultipleEEM}}
#'
#' @import ggplot2
#' @importFrom colorRamps matlab.like
#' @importFrom reshape2 melt 
#' 
#' @export
#' 
drawEEMgg <- function(x, ...) UseMethod("drawEEMgg", x)

#' @describeIn drawEEMgg draw EEM of EEM data created by \code{\link{readEEM}} function
#' 
#' @export
#'
drawEEMgg.EEM <-
  function(x, n, textsize = 20, color = matlab.like, geom = "path", 
           nlevel = 20, xlab = "Excitation wavelength [nm]", ylab = "Emission wavelength [nm]", title = NULL){

    # retrieve data 
    data <- x[[n]] # data is a matrix 
    
    # if title is not provided, call it  
    if (is.null(title)) title <- names(x)[n]
    
    # melt data 
    data.melted <- melt(data)
    names(data.melted) <- c("em", "ex", "value")
        
    # plot melted data
    drawEEMgg_internal(x = data.melted, n = n, textsize = textsize, 
                      color = color, geom = geom, 
                      nlevel = nlevel, xlab = xlab, ylab = ylab, 
                      title = title)
  }

#' @describeIn drawEEMgg draw contours of the output from \code{\link[EEM]{getLoading}} and 
#' \code{\link[EEM]{getReg}}. 
#' @export
drawEEMgg.EEMweight <-
    function(x, ncomp, textsize = 20, color = matlab.like, geom = "path", 
             nlevel = 50, xlab = "Excitation wavelength [nm]", ylab = "Emission wavelength [nm]", title = NULL){
        
        # check inputs such that ncomp cannot exceed totalcomp
        totalcomp <- dim(x$value)[2]
        if (ncomp > totalcomp) stop("ncomp cannot exceed totalcomp.")
        
        # extract data from x
        data <- x$value[,ncomp]
        id <- names(data)
        
        # extract ex and em information from colnames
        if (isTRUE(grepl("^EX...EM...", id[1]))){
            ex <- substring(id, 3, 5)
            em <- substring(id, 8, 10)
        } else if (isTRUE(grepl("EX...EM...", id[1]))){
            pattern <- "EX...EM..."
            m <- regexpr(pattern, id)
            id <- regmatches(id, m)
            ex <- substring(id, 3, 5)
            em <- substring(id, 8, 10)
        } else {
            stop("Input did not follow the format.")
        }
        
        # melt data
        data.melted <- data.frame(em = as.numeric(em), ex = as.numeric(ex), 
                                  value = data, row.names = NULL)
        
        # title
        if (is.null(title)) {
            title <- x$title #if title is not provided, call it
            title <- paste(title, " (", ncomp, " LV)", sep = "")
            if (ncomp > 1) title <- sub("LV", "LVs", title)
        } 
        
        # plot melted data
        drawEEMgg_internal(x = data.melted, n = n, textsize = textsize, 
                           color = color, geom = geom, 
                           nlevel = nlevel, xlab = xlab, ylab = ylab, 
                           title = title)
    }

#' @export
drawEEMgg_internal <-
    function(x, n = n, textsize = textsize, 
             color = color, geom = geom, 
             nlevel = nlevel, xlab = xlab, ylab = ylab, 
             title = title){
        
        # get ranges        
        ex.range <- range(x$ex, na.rm = TRUE)
        em.range <- range(x$em, na.rm = TRUE)
        value.range <- range(x$value, na.rm = TRUE)
        
        if (geom == "polygon"){
            
            # prepare to extend grid and add in arbitary value
            arbitaryValue = value.range[1]-
                (diff(value.range)/nlevel) * 1.5
            
            tmp1 <- data.frame(ex = ex.range[1]-1, em = unique(x$em), value = arbitaryValue)
            tmp2 <- data.frame(ex = unique(x$ex), em = em.range[1]-1, value = arbitaryValue)
            tmp3 <- data.frame(ex = ex.range[2]+1, em = unique(x$em), value = arbitaryValue)
            tmp4 <- data.frame(ex = unique(x$ex), em = em.range[2]+1, value = arbitaryValue)
            tmp <- rbind(tmp1, tmp2, tmp3, tmp4)
            dataForPlotting <- rbind(x, tmp)
            
            # since stat_contour cannot accept NA values, if NA values are present replace them with zero
            dataForPlotting$value[is.na(dataForPlotting$value)] <- 0
            
            # draw EEM
            v <- ggplot(dataForPlotting, aes(x = ex, y = em, z = value)) + 
                stat_contour(geom = "polygon", aes(fill = ..level..), bins = nlevel) +
                theme(panel.grid = element_blank(),   # delete grid lines
                      panel.border = element_rect(colour = "grey50", fill=NA)) +
                scale_fill_gradientn(colours = color(nlevel)) +
                coord_cartesian(xlim = c(ex.range[1],ex.range[2]),
                                ylim = c(em.range[1],em.range[2])) 
            
        } else {
            v <- ggplot(x, aes(x = ex, y = em, z = value)) + 
                stat_contour(geom = "path", aes(colour = ..level..), bins = nlevel) +
                scale_colour_gradientn(colours = color(nlevel)) +
                coord_cartesian(xlim = c(ex.range[1],ex.range[2]),
                                ylim = c(em.range[1],em.range[2])) 
        }
        
        # add some themes to the plot
        w <- v +           
            theme(panel.grid = element_blank(),  # delete grid lines 
                  text = element_text(size = textsize),  # change all text size
                  panel.background = element_rect(fill = 'white'),  # white bg
                  panel.border = element_rect(colour = "grey50", fill = NA)) +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(title) +
            guides(fill = guide_colorbar(title = "intensity")) 
        
        return(w)
    }