#' Convert Long Data to Wide
#' @description Convert long data to wide.
#' @param data long data
#' @param id can be one or more. If id is missing, row numbers will be treated as id
#' @param prefix prefix of value variables
#' @param suffix suffix of value variables
#' @param j the name of operated column
#'
#' @return wide data
#' @export
#'
#' @examples
#' df = data.frame(id = c(1,1,2,2,3,3,4,4),
#'                 time = c(1,2,1,2,1,2,1,2),
#'                 w = c(1,6,2,7,3,8,4,9),
#'                 h = c(5,1,6,3,7,5,8,7),
#'                 n = c(2,2,3,3,4,4,5,5))
#' 
#' # prefix
#' reshape_toWide(data = df,
#'                id='m',
#'                prefix = c('w','h'),
#'                j = 'time')
#' 
#' # suffix
#' reshape_toWide(data = df,
#'                id='m',
#'                suffix = c('w','h'),
#'                j = 'time')
reshape_toWide <- function(data,id,prefix,suffix,j){
  if (!is.data.frame(data)){
    data=data.frame(data)
  }
  data = data[order(data[,j]),]
  #trans-data
  #the other part except prefix
  #prefix
  if (all(!missing(prefix),missing(suffix))){
    location=c(prefix,j) %!=% colnames(data)
    id.left.names=colnames(data)[location[duplicated(location)]]
    trans.data=data[,c(j,prefix)]
  }else if (all(missing(prefix),!missing(suffix))){
    #suffix
    location=c(suffix,j) %!=% colnames(data)
    id.left.names=colnames(data)[location[duplicated(location)]]
    trans.data=data[,c(j,suffix)]
  }else if (all(missing(prefix),missing(suffix))){
    #var.names
    location=j %!=% colnames(data)
    id.left.names=colnames(data)[location[duplicated(location)]]
    trans.data=data[,j]
  }else{
    stop('prefix, suffix can not be used together.')
  }
  if (length(id.left.names)==1){
    do.id = data[,id.left.names]
  }else if (length(id.left.names)>1){
    do.id = data[,id.left.names]
    for (i in 1:length(id.left.names)) {
      if (i == 1){
        do.id=data[,id.left.names[i]]
      }else{
        do.id=paste0(do.id,'idsplitid',data[,id.left.names[i]])
      }
    }
  }else if (length(id.left.names)==0){
    idsplitid.j=make.unique(as.character(trans.data[,j]),sep = 'idsplitid')
    idsplitid.j=reshape2::colsplit(idsplitid.j,'idsplitid',c('j','id'))
    do.id = ifelse(is.na(idsplitid.j[,2]),0,idsplitid.j[,2]) +1
    id.left.names='nrow'
  }
  data2=cbind(do.id,trans.data)
  wide.1=stats::reshape(data = data2, 
          idvar = 'do.id',
          timevar = j, 
          direction = "wide",sep = "")
  wide.2=wide.1[,-1]
  if (!missing(suffix)){
    suffix=suffix[order(nchar(suffix),decreasing = TRUE)]
    #location of suffix and trans suffix to prefix
    for (i in 1:length(suffix)) {
      suffix.char=left(colnames(wide.2),nchar(suffix[i]))
      suffix.loc=(1:length(suffix.char))[suffix[i] == suffix.char]
      name.suffix=Replace0(colnames(wide.2)[suffix.loc],suffix[i])
      colnames(wide.2)[suffix.loc]=paste0(name.suffix,suffix[i])
    }
  }
  wide.3=reshape2::colsplit(wide.1[,1],'idsplitid',id.left.names)
  if (is.data.frame(wide.3)){
    wide.3=data.frame(wide.3)
    colnames(wide.3)=id.left.names
  }
  wide=cbind(wide.3,wide.2)
  return(wide)
}
