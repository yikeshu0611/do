#' Find Common Objects from Vectors
#'
#' @param ... must be several vectors
#'
#' @return common objects
#' @export
#'
#' @examples 
#' 
#' x1=c('a','e','d')
#' x2=c('a','c','e')
#' x3=c('a','e','j','d')
#' common(x1,x2,x3)
#' 
common <- function(...){
    x=list(...)
    for (i in 1:length(x)) {
        if (i==1){
            common = unlist(x[i])
        }else{
            common=unlist(x[i])[unique(unlist(common %==% unlist(x[i])))]
        }
    }
    common
}
