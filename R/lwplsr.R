lwplsr <- function(
    X, Y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv
    ) {
    structure(
        list(X = X, Y = Y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv),
        class = "Lwplsr"
        )
    }
    
    
.predict_Lwpls <- function(object, X, ..., nlv = NULL, fun) {
    X <- .mat(X)
    A <- object$nlv
    if(is.null(nlv))
        nlv <- A 
    else 
        nlv <- seq(min(nlv), min(max(nlv), A))
    le_nlv <- length(nlv)
    if(is.null(object$Y))
        da <- TRUE
    else
        da <- FALSE
    ## Getknn
    if (object$nlvdis == 0)
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
    else {
        if(da)
            object$Y <- dummy(object$y)$Y
        fm <- plskern(object$X, object$Y, object$nlvdis)
        res <- getknn(fm$T, transform(fm, X), k = object$k, diss = object$diss)
        }
    ## End
    listw <- lapply(res$listd, wdist, h = object$h)    
    if(da)
        object$Y <- object$y
    pred <- locwlv(object$X, object$Y, X,
        listnn = res$listnn, listw = listw, fun = fun, nlv = nlv)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
    }

predict.Lwplsr <- function(object, X, ..., nlv = NULL)
    .predict_Lwpls(object, X, ..., nlv = nlv, fun = plskern)
    






