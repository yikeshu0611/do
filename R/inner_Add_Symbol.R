#' Concatenate Strings
#' @description Concatenate vectors by adding a symbol.
#' @param x vectors
#' @param symbol defulat is '+'
#'
#' @return a concatenated string
#' @export
#'
#' @examples
#' inner_Add_Symbol(c('a','b'))
#' inner_Add_Symbol(c('a','b'),"$")
#' inner_Add_Symbol(c('a','b'),"")
inner_Add_Symbol <- function(x,symbol="+"){
  if (length(x)>=2){
    for (x.i in 1:length(x)) {
      if (x.i==1){
        adj=x[1]
      }else{
        adj=paste0(adj,symbol,x[x.i])
      }
    }
  }else{
    adj=x
  }
  adj
}
