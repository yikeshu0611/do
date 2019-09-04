#' Convert Wide Data to Long
#' @description It is easy to convert wide data to long in this function. Be careful, id must be unique. prefix, suffix and var.names can be used together.
#' @param data wide data
#' @param id can be one or more. If id is missing, row numbers will be treated as id
#' @param prefix prefix of value variables
#' @param suffix suffix of value variables
#' @param var.names names of value variables, do.value will be created as the name of value column
#' @param j a name of new column
#'
#' @return long data
#' @export
#'
#' @examples
#' # id is missing
#' df = data.frame(
#'   w1 = c(1,2,3,4),
#'   w2 = c(6,7,8,9),
#'   h1 = c(5,6,7,8),
#'   h2 = c(1,3,5,7),
#'   names = c('s1','s2','s3','s4')
#' )
#' reshape_toLong(data = df,prefix = c('w','h'),j = "time")
#' 
#' # id is not missing
#' df = data.frame(
#'   id = c(101,102,103,104),
#'   w1 = c(1,2,3,4),
#'   w2 = c(6,7,8,9),
#'   h1 = c(5,6,7,8),
#'   h2 = c(1,3,5,7),
#'   names = c('s1','s2','s3','s4')
#' )
#' reshape_toLong(data = df,id = 'id',prefix = c('w','h'),j = "time")
#' 
#' # two ids
#' df = data.frame(
#'   id1 = c(101,101,102,102),
#'   id2 = c('female','male','female','male'),
#'   w1 = c(1,2,3,4),
#'   w2 = c(6,7,8,9),
#'   h1 = c(5,6,7,8),
#'   h2 = c(1,3,5,7)
#' )
#' reshape_toLong(data = df,id = c('id1','id2'),prefix = c('w','h'),j = "time")
#' 
#' # using suffix
#' df = data.frame(
#'   id = c(102,103,104,105),
#'   t1w = c(1,2,3,4),
#'   t2w = c(6,7,8,9),
#'   t1h = c(5,6,7,8),
#'   t2h = c(1,3,5,7),
#'   sex=c('female','male','male','female')
#' )
#' reshape_toLong(data = df,id = 'id',suffix = c('w','h'),j = "time")
#' 
#' # using prefix and suffix together
#' df = data.frame(
#'   id = c(102,103,104,105),
#'   wt1 = c(1,2,3,4),
#'   wt2 = c(6,7,8,9),
#'   t1h = c(5,6,7,8),
#'   t2h = c(1,3,5,7),
#'   sex=c('female','male','male','female')
#' )
#' reshape_toLong(data = df,id = 'id',prefix = 'w',suffix = 'h',j = "time")
#' 
#' # using var.names
#' df = data.frame(
#'   id = c(102,103,104,105),
#'   w = c(1,2,3,4),
#'   h = c(1,3,5,7),
#'   sex=c('female','male','male','female')
#' )
#' reshape_toLong(data = df,id = 'id',var.names = c('w','h'),j = "wh")

reshape_toLong <- function(data,id,prefix,suffix,var.names,j){
  if (length(j)>1) stop('the length of j must be 1')
  #divide data into 3 parts:
  #id.data
  #trans.data
  #left.data
###get id-data
  if (missing(id)){
    id.names='nrow'
    id.p = 1:nrow(data)
    id.data=id.p
  }else{
    id.names=id
    id.no=unlist(id %==% colnames(data))
    id.data=data[,id.no]
    if (length(id.no)>1){
      for (i in 1:length(id.no)) {
        if (i==1) {
          id.p=id.data[,1]
        }else{
          id.p=paste0(id.p,'idsplitid',id.data[,i])
        }
      }
    }else{
      id.p=id.data
    }
    id.data=id.p
  }
  if (any(duplicated(data[,id]))) stop(paste(id,"is not unique"))
###get trans-data
  #suffix
  if (!missing(suffix)){
    suffix=suffix[order(nchar(suffix),decreasing = TRUE)]
    #location of suffix and trans suffix to prefix
    for (i in 1:length(suffix)) {
      suffix.char=right(colnames(data),nchar(suffix[i]))
      suffix.loc=(1:length(suffix.char))[suffix[i] == suffix.char]
      name.suffix=Replace0(colnames(data)[suffix.loc],suffix[i])
      colnames(data)[suffix.loc]=paste0(suffix[i],name.suffix)
    }
  }
  #names
  if (!missing(var.names)){
    names.loc=unlist(var.names %==% colnames(data))
    colnames(data)[names.loc]=paste0('do.value',var.names)
    'do.value'
  }
  #build prefix
  if (all(!missing(prefix),!missing(suffix),!missing(var.names))){
    prefix=c(prefix,suffix,'do.value')
  }
  if (all(!missing(prefix),!missing(suffix),missing(var.names))){
    prefix=c(prefix,suffix)
  }
  if (all(!missing(prefix),missing(suffix),!missing(var.names))){
    prefix=c(prefix,'do.value')
  }
  if (all(!missing(prefix),missing(suffix),missing(var.names))){
    prefix=prefix
  }
  if (all(missing(prefix),!missing(suffix),!missing(var.names))){
    prefix=c(suffix,'do.value')
  }
  if (all(missing(prefix),!missing(suffix),missing(var.names))){
    prefix=suffix
  }
  if (all(missing(prefix),missing(suffix),!missing(var.names))){
    prefix='do.value'
  }
  prefix=prefix[order(nchar(prefix),decreasing = TRUE)]
  for (i in 1:length(prefix)) {
    if (i==1) trans.no=c()
    prefix.i = prefix[i]
    left.char=left(colnames(data),nchar(prefix.i))
    trans.no=c(trans.no,prefix.i %==% left.char)
    if (i == length(prefix)){
      trans.no=unique(trans.no)
    }
  }
  trans.data=data[,trans.no]
###get left-data
  left.names=Replace0(data = colnames(data),
                    from = c(id.names,colnames(data)[trans.no]))
  left.names.all0=all(nchar(left.names)==0)
  if (left.names.all0){
    left.p='no.left.data'
  }else{
    if (missing(id)){
      left.names=colnames(data)[-trans.no]
    }else{
      left.names=colnames(data)[-c(trans.no,id.no)]
    }
    if (length(left.names)==0){
      left.p=0
    }else if (length(left.names)==1){
      left.p=data[,left.names]
    }else if (length(left.names)>1){
      left.names.data=data[,left.names]
      for (i in 1:length(left.names)) {
        if (i==1) {
          left.p=left.names.data[,1]
        }else{
          left.p=paste0(left.p,'idsplitid',left.names.data[,i])
        }
      }
      }
  }
  left.data=left.p
##########################reshape to long
  for (i in 1:length(prefix)) {
    if (i==1) {
      trans.colnames=colnames(trans.data)
      prefix.char=left(trans.colnames,nchar(prefix[i]))
      prefix.short=prefix.char[prefix[i] == prefix.char]
      trans.colnames=trans.colnames[prefix[i] != prefix.char]
      prefix.char=prefix.char[prefix[i] != prefix.char]
    }else{
      prefix.char=left(trans.colnames,nchar(prefix[i]))
      prefix.short=c(prefix.short,prefix.char[prefix[i] == prefix.char])
      trans.colnames=trans.colnames[prefix[i] != prefix.char]
      prefix.char=prefix.char[prefix[i] != prefix.char]
    }
  }
  for (i in 1:length(prefix)) {
    prefix.i=prefix[i]
    long.i.data=trans.data[,prefix.i==prefix.short]
    long.i.names=colnames(trans.data)[prefix.i==prefix.short]
    if (!is.data.frame(long.i.data)){
      long.i.data=data.frame(long.i.data)
      colnames(long.i.data)=long.i.names
    }
    long.i.number=Replace0(data = long.i.names,prefix.i)
    for (m in 1:length(long.i.number)) {
      if (m==1){
        long.1=cbind(long.i.number[m],long.i.data[,m])
        long.1[,1]=paste0(id.data,'IDANDVAR',long.1[,1])
      }else{
        long.2=cbind(long.i.number[m],long.i.data[,m])
        long.2[,1]=paste0(id.data,'IDANDVAR',long.2[,1])
        long.2=data.frame(long.2)
        long.1=data.frame(long.1)
        long.1=plyr::rbind.fill(long.1,long.2)
      }
      if (m==length(long.i.number)){
        colnames(long.1)=c(j,prefix.i)
      }
    }
    if (i==1){
      long.trans.get=long.1
    }else{
      long.trans.get=merge(x = long.trans.get,y = long.1,all=TRUE)
    }
  }
  order.1 = long.trans.get[,1]
  long.trans.get=long.trans.get[order.1[order(as.character(order.1))],]
  #get long data
  if (all(length(left.data)==1,
          left.data=='no.left.data')){
    split.col=reshape2::colsplit(long.trans.get[,1],'IDANDVAR',c('id.data',j))
    jcol=data.frame(split.col[,2])
    colnames(jcol)=j
    id.p.col=split.col[,1]
    id.all.cols=reshape2::colsplit(split.col[,1],'idsplitid',id.names)
    long.trans.df=long.trans.get[,-1]
    if (!is.data.frame(long.trans.df)){
      long.trans.df=data.frame(long.trans.df)
      colnames(long.trans.df)=prefix
    }
    long.data=cbind(id.all.cols,jcol,long.trans.df)
    row.names(long.data)=1:nrow(long.data)
    return(long.data)
  }else{
    split.col=reshape2::colsplit(long.trans.get[,1],'IDANDVAR',
                                 c('id.data',j))
    long.trans.df=long.trans.get[,-1]
    if (!is.data.frame(long.trans.df)){
      long.trans.df=data.frame(long.trans.df)
      colnames(long.trans.df)=prefix
    }
    long.id.trans=cbind(split.col,long.trans.df)
    left.df=cbind(id.data,left.data)
    long.id.trans.left=merge(long.id.trans,left.df,all=TRUE)
    id.all.cols=reshape2::colsplit(long.id.trans.left[,1],'idsplitid',id.names)
    left.all.cols=reshape2::colsplit(long.id.trans.left[,ncol(long.id.trans.left)],'idsplitid',left.names)
    long.data=cbind(id.all.cols,long.id.trans.left[,-c(1,ncol(long.id.trans.left))],left.all.cols)
    return(long.data)
  }
}

