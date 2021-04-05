#' @rdname Target
#' @export
#'
#' @examples
#' \donttest{
#' # get file url
#' Target_download_dir('https://target-data.nci.nih.gov/Public/ALL/clinical/')
#' }
Target_download_url <- function(url){
    dir_loop(url)
}