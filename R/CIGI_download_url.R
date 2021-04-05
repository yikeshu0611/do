
#' @rdname CIGI
#' @export
#'
#' @examples
#' \donttest{
#' # get all downloaded urls
#' CIGI_download_url(url='https://cigi-data.nci.nih.gov/Public/')
#' }
CIGI_download_url <- function(url){
    dir_loop(url)
}
