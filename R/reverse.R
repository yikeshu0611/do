#' Reverse String Order
#'
#' @param x can be number, strings, verctors, dataframe or matrix.
#'
#' @return reversed string
#' @export
#'
#' @examples 
#' reverse(123)
#' reverse(c(123,'abc'))
#' 
#' df = data.frame(
#'   a = c(123, 456),
#'   b = c("abc", "def")
#' )
#' reverse(df)
reverse <- function(x){
  #creat a single function
  Reverse.i <- function(pattern,joint=""){
      for (i in 1:nchar(pattern)) {
        if (i==1){pattern.r=c()}
        pattern.r=c(mid(pattern,i,1),pattern.r)
      }
      inner_Add_Symbol(pattern.r,joint)
  }
  if (any(is.data.frame(x),is.matrix(x))){
    for (i in 1:ncol(x)) {
      x.i=as.character(x[,i])
      x[,i]=unlist(lapply(x.i,Reverse.i))
    }
    x
  }else{
    unlist(lapply(x,Reverse.i))
  }
}
