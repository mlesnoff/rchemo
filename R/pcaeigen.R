pcaeigen <- function(X, weights = NULL, nlv) {
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    nlv <- min(nlv, n, p)
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    xmeans <- .colmeans(X, weights = weights)
    X <- .center(X, xmeans)
    res <- eigen(crossprod(sqrt(weights) * X), symmetric = TRUE)
    ## X' * D * X = V' * Delta^2 * V
    ## P = V
    P <- res$vectors[, seq_len(nlv), drop = FALSE]
    eig <- res$values[seq_len(min(n, p))]
    eig[eig < 0] <- 0
    sv <- sqrt(eig)
    T <- X %*% P
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
    colnames(T) <- colnames(P) <-  paste("pc", seq_len(nlv), sep = "")
    structure(
        list(T = T, P = P, sv = sv, eig = eig,
            xmeans = xmeans, weights = weights, niter = NULL, conv = NULL),
        class = c("Pca"))
}




