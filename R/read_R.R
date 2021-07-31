#' Read R file
#'
#' @param R path of R file
#'
#' @return one vector of R command with names of R file
#' @export
#'
read_R <- function(R){
    r <- list.files(path = R,pattern = '\\.R',full.names = TRUE)
    x <- sapply(r, function(i){
        readLines(i) |> paste0(collapse = '\n')
    })
    names(x) <- file.name(r)
    x
}
