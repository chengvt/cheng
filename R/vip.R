#' @export
vip <- function(model){
    source("http://mevik.net/work/software/VIP.R")
    return(VIP(model))
}
