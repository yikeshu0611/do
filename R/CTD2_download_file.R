
#' @rdname CTD2
#' @export
#'
#' @examples
#' \donttest{
#' # download files by file url
#' CTD2_download_file(url='https://ctd2-data.nci.nih.gov/Public/Broad/CTRPv1.0_2013_pub_Cell_154_1151/CTRPv1.0._COLUMNS.xlsx')
#' }
CTD2_download_file <- function(url){
    to.down(url)
}
