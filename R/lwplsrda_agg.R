lwplsrda_agg <- function(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv, 
    verb = FALSE) {
    diss <- match.arg(diss)
    structure(
        list(X = X, Y = y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv, verb = verb),
        class = "Lwplsrda_agg")
    }
    
predict.Lwplsrda_agg <- function(object, X, ...) {
    X <- .mat(X)
    ## Getknn
    if (object$nlvdis == 0)
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
    else {
        zfm <- plskern(object$X, object$y, nlv = object$nlvdis)
        res <- getknn(zfm$T, transform(zfm, X), k = object$k, diss = object$diss)
        }
    listw <- lapply(res$listd, wdist, h = object$h)    
    ## End
    pred <- locw(object$X, object$y, X,
        listnn = res$listnn, listw = listw,
        fun = plsr_agg, nlv = object$nlv, verb = object$verb)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
    }







