lwplsr_agg <- function(
    X, Y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv, 
    cri = 4,
    verb = FALSE) {
    diss <- match.arg(diss)
    structure(
        list(X = X, Y = Y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv, cri = cri, verb = verb),
        class = "Lwplsr_agg")
}
    
predict.Lwplsr_agg <- function(object, X, ...) {
    X <- .mat(X)
    ## Getknn
    if (object$nlvdis == 0)
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
    else {
        zfm <- plskern(object$X, object$Y, nlv = object$nlvdis)
        res <- getknn(zfm$T, transform(zfm, X), k = object$k, diss = object$diss)
    }
    listw <- lapply(res$listd, wdist, h = object$h, cri = object$cri)    
    # for stabilization
    tol <- 1e-5
    foo <- function(x) {x[x < tol] <- tol ; x}
    listw <- lapply(listw, foo) 
    ## End
    pred <- locw(object$X, object$Y, X,
        listnn = res$listnn, listw = listw,
        fun = plsr_agg, nlv = object$nlv, 
        verb = object$verb)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
}







