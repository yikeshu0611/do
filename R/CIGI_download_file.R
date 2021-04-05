
#' @rdname CIGI
#' @export
#'
#' @examples
#' \donttest{
#' # download file by url
#' CIGI_download_file(url='https://cgci-data.nci.nih.gov/Public/BLGSP/biospecimen/BCCA/CGCI-BLGSP_BiospecimenSupplementSpreadsheet001_20191011.xlsx')
#' }
CIGI_download_file <- function(url){
    dir_loop(url)
}
