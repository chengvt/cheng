#' knit .rmd to .md file for github.io
#' @export
knitPost <- function(input, base.url = "/") {
    require(knitr)
    opts_knit$set(base.url = base.url)
    fig.path <- paste0("../knitr-figs/", sub(".Rmd$", "", basename(input)), "/")
    opts_chunk$set(fig.path = fig.path)
    opts_chunk$set(fig.cap = "center")
    render_jekyll()
    print(paste0("../_posts/", sub(".Rmd$", "", basename(input), ignore.case = TRUE), ".md"))
    knit(input, output = paste0("../_posts/", sub(".Rmd$", "", basename(input), ignore.case = TRUE), ".md"), 
         envir = parent.frame())
}
