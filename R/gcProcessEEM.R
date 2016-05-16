#' Group and column-wise process EEM data matrix
#' 
#' Group and column-wise process EEM data matrix
#' 
#' @param EEM EEM object
#' @param group a character or factor vector
#' @param FUN function to apply
#' 
#' @examples 
#' require(EEM)
#' data(applejuice)
#' applejuice_mean <- gcProcessEEM(applejuice, country, mean)
#' drawEEM(applejuice_mean, 1)
#' 
#' @export
gcProcessEEM <- function(EEM, group, FUN){
    
    EEM_uf <- unfold(EEM)
    EEM_uf_applied <- aggregate(EEM_uf, by = list(group), FUN)
    
    # move the first column to row names
    rownames(EEM_uf_applied) <- EEM_uf_applied[,1]
    EEM_uf_applied <- EEM_uf_applied[,2:ncol(EEM_uf_applied)]
    
    EEM_applied <- fold(EEM_uf_applied)
    return(EEM_applied)
}

