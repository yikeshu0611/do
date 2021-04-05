#' Download data from CIGI database
#'
#' @param url url link of data
#' @name CIGI
#' @importFrom magrittr %>%
#' @return downloaded data
#' @export
#'
#' @examples
#' \donttest{
#' CIGI_download('https://cigi-data.nci.nih.gov/Public/')
#' }
CIGI_download <- function(url){
    message('=====     Achieving all urls     =====')
    url <- dir_loop(url)
    message('\n=====     Downloading     =====')
    to.down(url)
}