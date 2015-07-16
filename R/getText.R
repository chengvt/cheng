#' @export
getText <- function(text, n, sep = "_"){
    sapply(strsplit(text, split = sep), "[", n)
}