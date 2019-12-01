#' Split One Column and Expand
#'
#' @param data dataframe or matrix
#' @param variable one column name with connected values
#' @param sep seperated symbol
#'
#' @return expanded dataframe or matrix
#' @export
#'
#' @examples
#' df=data.frame(a=c(1,0),
#'               b=c('a','n'),
#'            cyl=c('6;6;4;4;4',
#'                  '6;8;'))
#' split_expand(data=df,variable='cyl',sep=';')
split_expand <- function(data,variable,sep){
    res=NULL
    for (i in 1:nrow(data)) {
        df.i=t(col_split(data[i,variable],sep))
        rownames(df.i)=NULL
        df.rbind=suppressWarnings(cbind(data[i,not(colnames(data),variable)],df.i))
        res=rbind(res,df.rbind)
    }
    rownames(res)=NULL
    colnames(res)=c(not(colnames(data),variable),variable)
    if (is.matrix(data)){
        return(res)
    }else if(is.data.frame(data)){
        res=data.frame(res,check.rows = FALSE)
        return(res)
    }
}
