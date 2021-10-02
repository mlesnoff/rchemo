lwplslda_agg <- function(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv, 
    prior = c("unif", "prop"),
    cri = 4,
    verb = FALSE
    ) {
    diss <- match.arg(diss)
    prior <- match.arg(prior)
    structure(
        list(X = X, y = y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv, 
             typda = "lda", prior = prior, cri = cri, verb = verb),
        class = "Lwplsprobda_agg")
}
 
lwplsqda_agg <- function(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    prior = c("unif", "prop"),
    cri = 4,
    verb = FALSE
    ) {
    diss <- match.arg(diss)
    prior <- match.arg(prior)
    structure(
        list(X = X, y = y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv, 
             typda = "qda", prior = prior, cri = cri, verb = verb),
        class = "Lwplsprobda_agg")
}

predict.Lwplsprobda_agg <- function(object, X, ...) {
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
    fun <- switch(object$typda, 
                  lda = plslda_agg, qda = plsqda_agg)
    pred <- locw(object$X, object$y, X,
        listnn = res$listnn, listw = listw,
        fun = fun, nlv = object$nlv, prior = object$prior, 
        verb = object$verb)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
}







