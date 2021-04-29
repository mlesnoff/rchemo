lwplsr_agg <- function(
    X, Y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv, 
    verb = FALSE
    ) {
    structure(
        list(X = X, Y = Y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv, verb = verb),
        class = "Lwplsragg"
        )
    }
    
predict.Lwplsragg <- function(object, X, ...) {
    X <- .mat(X)
    ## Getknn
    if (object$nlvdis == 0)
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
    else {
        zfm <- plskern(object$X, object$Y, object$nlvdis)
        res <- getknn(zfm$T, transform(zfm, X), k = object$k, diss = object$diss)
        }
    ## End
    listw <- lapply(res$listd, wdist, h = object$h)    
    pred <- locw(object$X, object$Y, X,
        listnn = res$listnn, listw = listw, fun = plsr_agg, nlv = object$nlv, 
        verb = object$verb)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
    }







