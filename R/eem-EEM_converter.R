#' @export
eem2EEM <- function(eem){
    EEM <- list()
    for (i in 1:length(eem)){
        EEM[[i]] <- eem[[i]][[2]]
        colnames(EEM[[i]]) <- eem[[i]][[3]]
        rownames(EEM[[i]]) <- eem[[i]][[4]]
        names(EEM)[i] <- eem[[i]][[1]]
    }
    class(EEM) <- "EEM"
    return(EEM)
}

#' @export
EEM2eem <- function(EEM){
    eem <- list()
    for (i in 1:length(EEM)){
        eem[[i]] <- list(sample = names(EEM)[i],
                         x = EEM[[i]],
                         ex = as.numeric(colnames(EEM[[i]])),
                         em = as.numeric(rownames(EEM[[i]]))
                         )
        class(eem[[i]]) <- "eem"
        attr(eem[[i]], "is_blank_corrected") <- FALSE
        attr(eem[[i]], "is_scatter_corrected") <- FALSE
        attr(eem[[i]], "is_ife_corrected") <- FALSE
        attr(eem[[i]], "is_raman_normalized") <- FALSE
    }
    names(eem) <- names(EEM)
    class(eem) <- "eemlist"
    return(eem)
}