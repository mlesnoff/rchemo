lwplsr <- function(
    X, Y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    verb = FALSE) {
    diss <- match.arg(diss)
    structure(
        list(X = X, Y = Y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv, verb = verb),
        class = "Lwplsr")
    }
    
predict.Lwplsr <- function(object, X, ..., nlv = NULL) {
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
        fm <- plskern(object$X, object$Y, nlv = object$nlvdis)
        res <- getknn(fm$T, transform(fm, X), k = object$k, diss = object$diss)
        }
    listw <- lapply(res$listd, wdist, h = object$h)    
    ## End
    pred <- locwlv(object$X, object$Y, X,
        listnn = res$listnn, listw = listw, 
        fun = plskern, nlv = nlv, verb = object$verb)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)    
    }








