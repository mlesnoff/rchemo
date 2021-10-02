lwplslda <- function(
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
        class = "Lwplsprobda")
}

lwplsqda <- function(
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
        class = "Lwplsprobda")
}

predict.Lwplsprobda <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    A <- object$nlv
    if(is.null(nlv))
        nlv <- A 
    else 
        nlv <- seq(max(1, min(nlv)), min(max(nlv), A))
    le_nlv <- length(nlv)
    ## Getknn
    if (object$nlvdis == 0)
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
    else {
        object$Y <- dummy(object$y)$Y
        fm <- plskern(object$X, object$Y, nlv = object$nlvdis)
        res <- getknn(fm$T, transform(fm, X), k = object$k, diss = object$diss)
    }
    listw <- lapply(res$listd, wdist, h = object$h, cri = object$cri)    
    # for stabilization
    tol <- 1e-5
    foo <- function(x) {x[x < tol] <- tol ; x}
    listw <- lapply(listw, foo) 
    ## End
    fun <- switch(object$typda, 
                  lda = plslda, qda = plsqda)
    pred <- locwlv(object$X, object$y, X,
                   listnn = res$listnn, listw = listw, 
                   fun = fun, nlv = nlv, prior = object$prior,
                   verb = object$verb)$pred
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)    
}
