#' @rdname Target
#' @export
#'
#' @examples
#' \donttest{
#' # download data by file url
#' Target_download_dir("https://target-data.nci.nih.gov/Public/OS/Brazil/gene_expression_array/METADATA/MANIFEST.txt")
#' }
Target_download_file <- function(url){
    to.down(url)
}