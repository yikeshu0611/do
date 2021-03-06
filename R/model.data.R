#' Extract data of model
#'
#' @param fit fitted results
#' @name model.data
#' @importFrom stats as.formula update
#' @return dataframe in the model
#' @export
#'
#' @examples
#' fit <- lm(mpg~vs+am+poly(qsec,2),data=mtcars)
#' head(model.data(fit))
#' model.y(fit)
#' model.x(fit)
model.data<-function(fit){
    if ('coxph' %in% class(fit)){
        formu1=paste0('~',paste0(model.x(fit),collapse = '+'))
        formu=as.formula(paste0('Surv(',paste0(model.y(fit),collapse = ','),')',formu1))
        fit2=update(object = fit,formu,model=TRUE,x=TRUE,y=TRUE)
        fit2$model$timeggg=as.numeric(fit2$model[,1])[1:nrow(fit2$model)]
        fit2$model$eventggg=as.numeric(fit2$model[,1])[-c(1:nrow(fit2$model))]
        colnames(fit2$model)[(ncol(fit2$model)-1):ncol(fit2$model)]=model.y(fit2)
        fit2$model=fit2$model[,-1]
    }else{
        formu=as.formula(paste0('.~',paste0(model.x(fit),collapse = '+')))
        fit2=update(object = fit,formu,model=TRUE,x=TRUE,y=TRUE)
    }
    fit2$model
}
#' @rdname model.data
#' @return
#' @export
#'
model.y <- function(fit){
    if ('coxph' %in% class(fit)){
        all.vars(fit$terms)[c(1,2)]
    }else{
        all.vars(fit$terms)[1]
    }
}
#' @rdname model.data
#' @return
#' @export
#'
model.x <- function(fit){
    if ('coxph' %in% class(fit)){
        all.vars(fit$terms)[-c(1,2)]
    }else{
        all.vars(fit$terms)[-1]
    }
}
