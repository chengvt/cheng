#' @export
getText <- function(text, n, sep = "_"){
    result <- sapply(strsplit(text, split = sep), "[", n)
    return(result)
}