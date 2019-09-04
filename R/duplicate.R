#' find all duplicated ones
#'
#' @param x can be number, strings, verctors, dataframe or matrix
#' @param column_between TRUE is to detect the duplicated rows.
#' @param row_between TRUE is to detect the duplicated columns.
#' @param row_in TRUE is to detect the duplicated values in each row.
#' @param column_in TRUE is to detect the duplicated values in each column.
#' @param every TRUE is to detect the duplicated values in dataframe or matrix.
#'
#' @return TRUE as duplicated, FALSE as not
#' @export
#'
#' @examples
#' x <- c('a','b','a',1,1,2)
#' duplicate(x)
#' a = data.frame(k=c(1,1,2,1,4),
#' l=c(1,1,'a',5,6))
#' duplicate(a,row_in = TRUE)
#' duplicate(a,row_between = TRUE)
#' duplicate(a,column_between = TRUE)
#' duplicate(a,column_in = TRUE)
duplicate <- function(x,column_between=FALSE,
                      row_between=FALSE,
                      row_in=FALSE,
                      column_in=FALSE,
                      every=FALSE){
    dup <- function(x){
        x %in% names(table(x))[table(x)>1]
    }
    if (any(is.data.frame(x),is.matrix(x))){
        if (row_between){
            for (i in 1:ncol(x)) {
                if (i ==1){
                    pst = as.character(x[,i])
                }else{
                    pst=paste0(pst,as.character(x[,i]))
                }
            }
            return(dup(pst))
        }
        if (column_between){
            for (i in 1:nrow(x)) {
                if (i ==1){
                    pst = as.character(x[i,])
                }else{
                    pst=paste0(pst,as.character(x[i,]))
                }
            }
            return(dup(pst))
        }
        if (row_in){
            dup.df = matrix(data = TRUE,nrow = nrow(x),
                            ncol = ncol(x))
            dup.df=as.data.frame(dup.df)
            for (i in 1:nrow(x)){
                dup.df[i,]=dup(as.character(x[i,]))
            }
            colnames(dup.df)=colnames(x)
            row.names(dup.df)=row.names(x)
            return(dup.df)
        }
        if (column_in){
            dup.df = matrix(data = TRUE,nrow = nrow(x),
                            ncol = ncol(x))
            dup.df=as.data.frame(dup.df)
            for (i in 1:ncol(x)){
                dup.df[,i]=dup(as.character(x[,i]))
            }
            colnames(dup.df)=colnames(x)
            row.names(dup.df)=row.names(x)
            return(dup.df)
        }
        if (every){
            for (i in 1:ncol(x)) {
                if (i ==1){
                    pst = as.character(x[,i])
                }else{
                    pst=c(pst,as.character(x[,i]))
                }
            }
            dup.pst=names(table(pst))[table(pst)>1]
            dup.df = matrix(data = TRUE,nrow = nrow(x),
                            ncol = ncol(x))
            dup.df=as.data.frame(dup.df)
            for (i in 1:ncol(x)) {
                dup.df[,i] = as.character(x[,i]) %in% dup.pst
            }
            colnames(dup.df)=colnames(x)
            row.names(dup.df)=row.names(dup.df)
            return(dup.df)
        }
        
    }else{
        return(dup(x))
    }
}

