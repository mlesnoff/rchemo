lwplslda <- function(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    prior = c("unif", "prop"),
    typda = c("lda", "qda"),
    verb = FALSE
    ) {
    diss <- match.arg(diss)
    prior <- match.arg(prior)
    typda <- match.arg(typda)
    structure(
        list(X = X, y = y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv, 
             prior = prior, typda = typda, verb = verb),
        class = "Lwplslda")
    }
    
predict.Lwplslda <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    A <- object$nlv
    if(is.null(nlv))
        nlv <- A 
    else 
        nlv <- seq(min(nlv), min(max(nlv), A))
    le_nlv <- length(nlv)
    ## Getknn
    if (object$nlvdis == 0)
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
    else {
        object$Y <- dummy(object$y)$Y
        fm <- plskern(object$X, object$Y, nlv = object$nlvdis)
        res <- getknn(fm$T, transform(fm, X), k = object$k, diss = object$diss)
    }
    listw <- lapply(res$listd, wdist, h = object$h)    
    ## End
    fun <- switch(object$typda, 
                  lda = plslda, qda = plsqda)
    pred <- locwlv(object$X, object$y, X,
                   listnn = res$listnn, listw = listw, 
                   fun = fun, nlv = nlv, prior = object$prior,
                   verb = object$verb)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)    
}
