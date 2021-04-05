#' Download data from CTD2 database
#'
#' @param url url link of data
#' @name CTD2
#' @importFrom magrittr %>%
#' @return downloaded data
#' @export
#'
#' @examples
#' \donttest{
#' CTD2_download_dir('https://ctd2-data.nci.nih.gov/Public/')
#' }
CTD2_download_dir <- function(url){
    message('=====     Achieving all urls     =====')
    url <- dir_loop(url)
    message('\n=====     Downloading     =====')
    to.down(url)
}