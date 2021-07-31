#' Test speed of mirror
#'
#' @param min.second the minium second time to visit the mirror web page
#' @param cran logical, whether to test CRAN mirrors. Default is TRUE
#' @param bioc logical, whether to test bioconductor mirrors. Default is TRUE
#' @importFrom utils read.csv
#' @return repositories which visiting time is minus the minium second.
#' @export
#'
mirror.speed <- function(min.second=0.2,
                         cran=TRUE,
                         bioc=TRUE){
    res=list()
    # CRAN
    if (cran){
        time=10
        cat(crayon::bgWhite('\n---test CRAN mirror---'))
        message('\nLoading CRAN mirrors')
        repo.test=read.csv(paste0(getOption('repos')['CRAN'],'CRAN_mirrors.csv'),stringsAsFactors = FALSE)$URL
        message('test ',length(repo.test),' mirrors')
        for (i in 1:length(repo.test)) {
            if (i==1) mirror=time.all=c()
            time=system.time({
                f=tryCatch(
                    httr::GET(url=repo.test[i],
                              httr::timeout(min.second)),
                    error=function(e) paste0(i,',')
                )
            })
            if (is.list(f)){
                mirror=c(mirror,repo.test[i])
                time.all=c(time.all,time[3])
                message('\n',repo.test[i],' ---',round(time[3],4),'s')
            }else{
                cat(f)
            }
        }
        res$cran=if(!is.null(time.all)) mirror[order(time.all)]
        if (length(time.all)>0){
            cat(sprintf('\nsuggest:  options(repos = c(CRAN="%s"))',mirror[which.min(time.all)[1]]))
        }
    }
    # bioc
    if (bioc){
        cat(crayon::bgWhite('\n\n---test bioconductor mirror---'))
        message('\nLoading bioconductor mirrors')
        repo.test=read.csv('https://bioconductor.org/BioC_mirrors.csv',stringsAsFactors = FALSE)$URL
        message('test ',length(repo.test),' mirrors')
        for (i in 1:length(repo.test)) {
            if (i==1) mirror=time.all=c()
            time=system.time({
                f=tryCatch(
                    httr::GET(url=repo.test[i],
                              httr::timeout(min.second)),
                    error=function(e) paste0(i,',')
                )
            })
            if (is.list(f)){
                mirror=c(mirror,repo.test[i])
                time.all=c(time.all,time[3])
                message('\n',repo.test[i],' ---',round(time[3],4),'s')
            }else{
                cat(f)
            }
        }
        res$bioc=if(!is.null(time.all)) mirror[order(time.all)]
        if (length(time.all)>0){
            cat(sprintf('\nsuggest:  options(repos = c(BioC_mirror="%s"))',mirror[which.min(time.all)[1]]))
        }
    }
    invisible(res)
}


#' Current mirror
#' @description Current mirrors of CRAN adn Bioconductor
#' @return a list contains CRAN and Bioconductor mirror
#' @export
#'
#' @examples
#' mirror.current()
mirror.current <- function(){
    res=list(CRAN=NA,bioc=NA)
    res['CRAN']=getOption('repos')
    res['bioc']=getOption('BioC_mirror')
    res
}
