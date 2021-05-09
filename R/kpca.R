kpca <- function(X, weights = NULL, nlv, kern = "krbf", ...) {
    X <- .mat(X)
    n <- dim(X)[1]
    nlv <- min(nlv, n)
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    kern <- eval(parse(text = kern))
    dots <- list(...)
    K <- kern(X, ...)
    ## If non-symmetric Gram matrix
    tK <- t(K)
    ## Kc = Phi * Phi', where Phi is centered
    Kc <- t(t(K - colSums(weights * tK)) - colSums(weights * tK)) + 
        sum(weights * t(weights * tK))
    ## Kd = D^(1/2) * Kc * D^(1/2) 
    ## = U * Delta^2 * U'
    Kd <- sqrt(weights) * t(sqrt(weights) * t(Kc))
    fm <- svd(Kd, nu = 0)
    U <- fm$v[, seq_len(nlv)]
    eig <- fm$d
    eig[eig < 0] <- 0
    sv <- sqrt(eig)
    P <- sqrt(weights) * .scale(U, scale = sv)
    ## T = Kc * P = D^(-1/2) * U * Delta
    T <- Kc %*% P
    ## = 1 / sqrt(weights) * .scale(U, scale = 1 / sv)
    colnames(T) <- colnames(P) <- paste("pc", seq_len(nlv), sep = "")
    structure(
        list(X = X, tK = tK, T = T, P = P, sv = sv, eig = eig,
            weights = weights, kern = kern, dots = dots),
        class = c("Kpca"))
}

summary.Kpca <- function(object, ...) {
    nlv <- dim(object$T)[2]
    TT <- object$weights * object$T * object$T
    tt <- colSums(TT)
    sstot <- sum(object$eig)
    pvar <- tt / sstot
    cumpvar <- cumsum(pvar)
    explvar <- data.frame(pc = seq(nlv), var = tt, pvar = pvar, cumpvar = cumpvar)
    row.names(explvar) <- seq(nlv)
    list(explvar = explvar)
}

transform.Kpca <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    a <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- a
    else 
        nlv <- min(nlv, a)
    weights <- object$weights
    ## Kc = Knew
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    Kc <- t(t(K - colSums(weights * t(K))) - colSums(weights * object$tK)) + 
        sum(weights * t(weights * object$tK))
    T <- Kc %*% object$P 
    T       
}







