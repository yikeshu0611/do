#' Reshape to Wide Format
#'
#' @param data long data
#' @param key column names for key, which can be one or more
#' @param value column names for exchange, which can be one or more
#' @param prefix column names for prefix, which can be one or more
#' @param suffix column names for suffix, which can be one or more
#' @param sep seperation
#' @return A wide data.
#' @export
#'
#' @examples
#'  df = data.frame(id = c(1,1,2,2,3,3,4,4),
#'               time = c(1,2,1,2,1,2,1,2),
#'               w = c(1,6,2,7,3,8,4,9))
#'  df
#'  reshape_toWide(data = df,
#'              key = 'time',
#'              prefix = 'w')
#'  df = data.frame(id = c(1,1,2,2,3,3,4,4),
#'        time = c(1,2,1,2,1,2,1,2),
#'        w = c(1,6,2,7,3,8,4,9),
#'        h = c(5,1,6,3,7,5,8,7),
#'        n = c(2,2,3,3,4,4,5,5))
#'  df
#'  reshape_toWide(data = df,
#'              key = 'time',
#'              prefix  = c('w','h','n'))
reshape_toWide <- function(data,key=NULL,value=NULL,prefix=NULL,
                           suffix=NULL,sep='_'){
    if (length(value)>1) prefix=value;value=NULL
    if (!is.null(value)){
        data.frame(tidyr::pivot_wider(data = data,
                               names_from  = tidyselect::all_of(key),
                               values_from = tidyselect::all_of(value)),
                   check.names = FALSE)
    }else if (!is.null(prefix)){
        if (length(prefix)==1){
            x=data.frame(tidyr::pivot_wider(data = data,
                                   names_from  = tidyselect::all_of(key),
                                   values_from = tidyselect::all_of(prefix)),
                       check.names = FALSE)
            colnames(x)[! colnames(x) %in% colnames(data)]=
                paste0(prefix,sep,colnames(x)[! colnames(x) %in% colnames(data)])
            x
        }else{
            x=lapply(prefix, function(i) data.frame(tidyr::pivot_wider(data = data[setdiff(colnames(data),setdiff(prefix,i))],
                                                                names_from  = tidyselect::all_of(key),
                                                                values_from = tidyselect::all_of(i)),
                                                    check.names = FALSE)
                     
                     )
            
            
            x.leave=setdiff(setdiff(colnames(data),key),prefix)
            for (i in 1:length(x)){
                colnames(x[[i]])[!colnames(x[[i]]) %in% x.leave] = paste0(prefix[i],sep,colnames(x[[i]])[!colnames(x[[i]]) %in% x.leave])   
                x[[i]]$iiddiidd=paste0_columns(x[[i]][x.leave],',,,,,,')
                x[[i]]=x[[i]][setdiff(colnames(x[[i]]),x.leave)]
                if (i==1){
                    xi=x[[i]]   
                }else{
                    xi=do::join_full(xi,x[[i]],'iiddiidd')
                }
            }
            xs=col_split(x = xi$iiddiidd,',,,,,,')
            colnames(xs)=x.leave
            xi=xi[setdiff(colnames(xi),'iiddiidd')]
            xc=cbind(xs,xi)
            xc[!NA.col.sums(xc)==nrow(xc)]
        }
        
    }else if (!is.null(suffix)){
        if (length(suffix)==1){
            x=data.frame(tidyr::pivot_wider(data = data,
                                   names_from  = tidyselect::all_of(key),
                                   values_from = tidyselect::all_of(suffix)),
                       check.names = FALSE)
            colnames(x)[! colnames(x) %in% colnames(data)]=
                paste0(colnames(x)[! colnames(x) %in% colnames(data)],sep,suffix)
            x
        }else{
            x=lapply(suffix, function(i) data.frame(tidyr::pivot_wider(data = data[setdiff(colnames(data),setdiff(suffix,i))],
                                            names_from  = tidyselect::all_of(key),
                                            values_from = tidyselect::all_of(i)),
                                            check.names = FALSE)
                     
                     
                     )
            x.leave=setdiff(setdiff(colnames(data),key),suffix)
            for (i in 1:length(x)){
                colnames(x[[i]])[!colnames(x[[i]]) %in% x.leave] = paste0(colnames(x[[i]])[!colnames(x[[i]]) %in% x.leave],sep,suffix[i])   
                x[[i]]$iiddiidd=paste0_columns(x[[i]][x.leave],',,,,,,')
                x[[i]]=x[[i]][setdiff(colnames(x[[i]]),x.leave)]
                if (i==1){
                    xi=x[[i]]   
                }else{
                    xi=do::join_full(xi,x[[i]],'iiddiidd')
                }
            }
            xs=col_split(x = xi$iiddiidd,',,,,,,')
            colnames(xs)=x.leave
            xi=xi[setdiff(colnames(xi),'iiddiidd')]
            xc=cbind(xs,xi)
            xc[!NA.col.sums(xc)==nrow(xc)]
        }
        
    }
}
