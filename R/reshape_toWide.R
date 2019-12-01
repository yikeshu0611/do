#' Reshape to Wide Format
#'
#' @param data long data
#' @param id column names for id, which can be one or more
#' @param col_change column names for exchange, which can be one or more
#' @param prefix column names for prefix, which can be one or more
#' @param suffix column names for suffix, which can be one or more
#' @param sep seperation
#'
#' @return A wide data.
#' @export
#'
#' @examples
#' df = data.frame(id = c(1,1,2,2,3,3,4,4),
#'                 time = c(1,2,1,2,1,2,1,2),
#'                 w = c(1,6,2,7,3,8,4,9))
#' 
#' reshape_towide(data = df,
#'                id = 'id',
#'                col_change = 'time',
#'                prefix = 'w')
#' df = data.frame(id = c(1,1,2,2,3,3,4,4),
#'          time = c(1,2,1,2,1,2,1,2),
#'          w = c(1,6,2,7,3,8,4,9),
#'          h = c(5,1,6,3,7,5,8,7),
#'          n = c(2,2,3,3,4,4,5,5))
#' 
#' reshape_towide(data = df,
#'                id = 'id',
#'                col_change = c('time','n'),
#'                prefix  = 'w',sep = '_')
#' 
#' reshape_towide(data = df,
#'                id = 'id',
#'                col_change = 'time',
#'                prefix  = c('w','h'))
#' 
#' reshape_towide(data = df,
#'                id = c('id','n'),
#'                col_change = 'time',
#'                prefix  = c('w','h'))
#' 
#' df = data.frame(id = c(1,1,2,2,3,3,4,4),
#'                 time = c('a','a','a','b','b','b','c','c'),
#'                 n=c(5,5,6,6,7,7,8,8))
#' reshape_towide(data = df,id = 'id',col_change = 'time')
#' reshape_towide(data = df,id = c('id','time'),col_change = 'n')
#' reshape_towide(data = df,id = 'id',col_change = c('time','n'))

reshape_towide <- function(data,id,col_change,prefix,suffix,sep='_'){
    for (i in 1:length(id)) {
        if (i==1){
            id_dd=as.character(data[,id[i]])
        }else{
            id_dd=paste0(id_dd,sep,data[,id[i]])
        }
    }
    for (i in 1:length(col_change)) {
        if (i==1){
            col_dd=as.character(data[,col_change[i]])
        }else{
            col_dd=paste0(col_dd,sep,data[,col_change[i]])
        }
    }
    if (missing(prefix)) prefix=NULL
    if (missing(suffix)) suffix=NULL
    var.names=NULL
    if (all(!is.null(prefix),!is.null(suffix))){
        fix=c(prefix,suffix)
    }else if(all(!is.null(prefix),is.null(suffix))){
        fix=prefix
    }else if(all(is.null(prefix),!is.null(suffix))){
        fix=suffix
    }else if(all(is.null(prefix),is.null(suffix))){
        var.names=1
        fix=as.character(Sys.time())
        id_names=fix
        data$as.characterSys.time=1
        colnames(data)[ncol(data)]=fix
    }
# trans data --------------------------------------------------------------
    data2=cbind(id_dd,col_dd,data[,fix])
    if (length(fix)==1) colnames(data2)[ncol(data2)]=fix
    data2=data.frame(data2,check.names = FALSE,stringsAsFactors = FALSE)
    res=suppressMessages(data.table::dcast(data.table::setDT(data2), id_dd~col_dd, 
          value.var=fix,sep = sep))
    if (is.null(var.names)){
        if (length(fix)==1){
            colnames(res)[-1]=paste0(fix,sep,colnames(res)[-1])
        }
        if (!is.null(suffix)){
            res.colname=colnames(res)
            suffix=suffix[order(nchar(suffix),decreasing = TRUE)]
            for (i in 1:length(suffix)) {
                judge=left(res.colname,nchar(suffix[i]))==suffix[i]
                judge[is.na(judge)]=FALSE
                new.name=paste0(mid(res.colname[judge],
                                    nchar(paste0(suffix[i],'\\',sep)),
                                    max(nchar(suffix))),sep,suffix[i])
                res.colname[judge]=NA
                colnames(res)[judge]=new.name
            }
        }
    }
# left data ---------------------------------------------------------------
    if(all(is.null(prefix),is.null(suffix))){
        left.data.names=not(colnames(data),c(id,col_change,prefix,suffix,id_names))
    }else{
        left.data.names=not(colnames(data),c(id,col_change,prefix,suffix))
    }
    if (length(left.data.names)==0) left.data.names=NULL
    left.data.names=c(id,left.data.names)
    left.dd=data[,left.data.names]
    if (!is.data.frame(left.dd)){
        left.dd=as.data.frame(left.dd)
        colnames(left.dd)=left.data.names
    }
    if (any(dim(left.dd)==0)) return(res[,-1])
    left.data=cbind(id_dd,left.dd)
    left.data=unique(left.data)
    final=merge(x = left.data,y = res,by = 'id_dd',all=TRUE)[,-1]
    return(final)
}
