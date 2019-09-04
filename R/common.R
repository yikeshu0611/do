#' Find Common Objects from Vectors
#'
#' @param x must be list
#'
#' @return common objects
#' @export
#'
#' @examples 
#' 
#' x1=c('a','e','d')
#' x2=c('a','c','e')
#' x3=c('a','e','j','d')
#' x=list(x1,x2,x3)
#' common(x)
#' 
common <- function(x){
    if (is.data.frame(x)) stop('x must be list')
    if (!is.list(x)) stop('x must be list')
    for (i in 1:length(x)) {
        if (i==1){
            common = unlist(x[i])
        }else{
            common=unlist(x[i])[unique(unlist(common %==% unlist(x[i])))]
        }
    }
    common
}
