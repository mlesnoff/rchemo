lwplsrda_agg <- function(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    cri = 4,
    verb = FALSE) {
    diss <- match.arg(diss)
    structure(
        list(X = X, y = y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv, cri = cri, verb = verb),
        class = "Lwplsrda_agg")
}
    
predict.Lwplsrda_agg <- function(object, X, ...) {
    X <- .mat(X)
    ## Getknn
    if (object$nlvdis == 0)
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
    else {
        object$Y <- dummy(object$y)$Y
        zfm <- plskern(object$X, object$Y, nlv = object$nlvdis)
        res <- getknn(zfm$T, transform(zfm, X), k = object$k, diss = object$diss)
    }
    listw <- lapply(res$listd, wdist, h = object$h, cri = object$cri)    
    # for stabilization
    tol <- 1e-5
    foo <- function(x) {x[x < tol] <- tol ; x}
    listw <- lapply(listw, foo) 
    ## End
    pred <- locw(object$X, object$y, X,
        listnn = res$listnn, listw = listw,
        fun = plsrda_agg, nlv = object$nlv, 
        verb = object$verb)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
}







