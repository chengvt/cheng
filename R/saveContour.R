#' Export all contours in FF object to png graphics
#' @export
saveContour <- function(FF, path = NULL, size = c("normal", "big"), rep = 1, 
                        width = NULL, height = NULL, pointsize = NULL, 
                        cex.lab = 1.3, cex.main = 1.3, ...){
    
    # set parameters
    if (length(size) > 1) size <- "normal"
    if (!is.null(width) & !is.null(height) & !is.null(pointsize)) {
        width <- width
        height <- height
        pointsize <- 32
    } else if (size %in% "normal"){
        width <- 600
        height <- 500
        pointsize <- 15
    } else if (size %in% "big"){
        width <- 1000
        height <- 900
        pointsize <- 32
    }
    
    # get names
    sName <- paste0(names(FF), ".png")
    if (!is.null(path)) {
        if (!file.exists(path)) {
            dir.create(file.path(getwd(), path))
            cat(paste0("A new folder '", path, "' has been created."))
            }
        sName <- file.path(path, sName)
        }
    
    # save
    J_index <- ((1:length(FF)) * rep) - (rep - 1)
    N <- length(J_index)
    for (i in 1:N){
        j <- J_index[i]
        png(sName[j], width = width, height = height, pointsize = pointsize)
        drawEEM(FF, j, cex.lab = cex.lab, cex.main = cex.main, ...)
        dev.off()
    }
}
