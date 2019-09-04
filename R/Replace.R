#' Replace
#' @description There are two methords in this function. You can use repalce many objects to one by form and to. pattern can be used to one object replaced by the other one.
#' @param data can be number, strings, verctors, dataframe or matrix.
#' @param from replaced stings
#' @param to replacements
#' @param pattern like from:to
#'
#' @return replaced data
#' @export
#'
#' @examples
#' Replace(data = 232,from = 2,to = 1)
#' Replace(data = c(232,'a4b'),
#'         from = c(2,'.*4'),to = 1,
#'         pattern = c('a:e','b:h'))
#' df = data.frame(
#'   a = c(232, 452),
#'   b = c("nba", "cba")
#' )
#' Replace(data = df,
#'         from = 2,to = 1,
#'         pattern = c('a:e','b:h'))
#' 
Replace <- function(data,from,to,pattern){
  Replace1<-function(data,from,to){
    if (any(is.data.frame(data),is.matrix(data))){
      for (i in 1:ncol(data)) {
        data[,i]=gsub(from,to,data[,i])
      }
    }else{
      data=gsub(from,to,data)
    }
    data
  }
  if (all(!missing(from),!missing(to))){
    for (i in 1:length(from)) {
      data=Replace1(data,from[i],to)
    }
  }
  if (!missing(pattern)){
    for (j in 1:length(pattern)) {
      from=gsub(":.*","",pattern[j])
      to=gsub(".*:","",pattern[j])
      data=Replace1(data,from,to)
    }
  }
data
}