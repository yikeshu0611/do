
#' Download data from Target database
#'
#' @param url url link of data
#' @name Target
#' @importFrom magrittr %>%
#' @return invisible url and downloaded data
#' @export
#'
#' @examples
#' \donttest{
#' Target_download_dir('https://target-data.nci.nih.gov/Public/ALL/clinical/')
#' }
Target_download_dir <- function(url){
    message('=====     Achieving all urls     =====')
    url <- dir_loop(url)
    message('\n=====     Downloading     =====')
    to.down(url)
}
to.down <- function(url){
    dir  = do::Replace0(url,c('.*-data.nci.nih.gov/Public/'))
    url2 = url[grepl('\\.',dir)]
    path2 = dir[grepl('\\.',dir)]
    file = do::reverse(do::Replace0(do::reverse(url2),'/.*'))
    for (i in 1:length(file)) path2[i]=do::Replace0(path2[i],file[i])
    for (i in 1:length(file)) {
        dirs = strsplit(path2[i],'/')[[1]]
        for (j in 1:length(dirs)) {
            dirj=paste0(dirs[1:j],collapse = '/')
            dir.create(dirj)
        }
        filenamei = paste0(dirj,'/',file[i])
        cat(paste0('[',i,'/',length(file),']'),
            paste0('[',which(file[path2 == path2[i]] == file[i]),'/',sum(path2 == path2[i]),']'),
            file[i],'\n')
        t1=Sys.time()
        download.file(url2[i],filenamei)
        t2=Sys.time()
        cat_difftime(t2-t1)
        cat('\n')
    }
}
cat_difftime <- function (x, digits = getOption("digits")){
    cat("Time difference of ", format(unclass(x), 
                                      digits = digits), " ", 
        attr(x, "units"), 
        "\n", sep = "")
}
target_dir <- function(url){
    f <- tryCatch(httr::GET(url = url,httr::timeout(60)),
                  error=function(e) 'e')
    f
    if (is.character(f)){
        cat('\nwait 3 minutes')
        # cat('\n')
        for (j in 1:3) {
            for (i in 1:6){
                cat(i)
                Sys.sleep(10)
            }
            cat('\n')
        }
        return(target_dir(url))
    }else{
        html = httr::content(f)
        all = html %>% 
            rvest::html_nodes(xpath = '//tr[@class]') %>%
            set::grep_or(c('class="even"','class="odd"')) %>%
            set::grep_not_and('Parent Directory') %>%
            rvest::html_nodes(xpath = 'td/a') %>% 
            set::grep_not_and('img') %>%
            rvest::html_text()
        names(all)=NULL
        return(all)
    }
    
}
dir_loop <- function(url){
    cat(url,'\n')
    dir1 = target_dir(url)
    dir = dir1[do::right(dir1,1) == '/']
    dir
    if (length(dir) >0){
        url2 = paste0(url,dir)
        for (i in 1:length(url2)) {
            if (i==1) linki=c()
            linki=c(linki,dir_loop(url=url2[i]))
        }
        return(linki)
    }else{
        if (length(dir1) > 0){
            link = paste0(url,dir1)
            return(link)
        }
        
    }
}
