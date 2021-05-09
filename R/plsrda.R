plsrda <- function(X, y, weights = NULL, nlv) {
    if(is.factor(y))
        y <- as.character(y)
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    nlv <- min(nlv, n, p)
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    z <- dummy(y)
    fm <- plskern(X, z$Y, weights = weights, nlv = nlv)
    structure(
        list(fm = fm, lev = z$lev, ni = z$ni),
        class = c("Plsrda"))       
}

predict.Plsrda <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    rownam <- row.names(X)
    colnam <- "y1"
    A <- dim(object$fm$P)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- seq(min(nlv), min(max(nlv), A))
    le_nlv <- length(nlv)
    posterior <- pred <- vector(mode = "list", length = le_nlv)
    for(i in seq_len(le_nlv)) {
        zposterior <- predict(object$fm, X, nlv = nlv[i])$pred
        z <- apply(zposterior, FUN = .findmax, MARGIN = 1)
        zpred <- matrix(.replace_bylev(z, object$lev), ncol = 1)
        dimnames(zpred) <- list(rownam, colnam)
        pred[[i]] <- zpred
        posterior[[i]] <- zposterior
    }
    names(posterior) <- names(pred) <- paste("lv", nlv, sep = "")
    if(le_nlv == 1) {
        pred <- pred[[1]] 
        posterior <- posterior[[1]]
    }
    list(pred = pred, posterior = posterior)
}

