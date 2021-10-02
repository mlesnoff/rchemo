fda <- function(X, y, nlv = NULL) {
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    if(is.factor(y))
        y <- as.character(y)
    xmeans <- colMeans(X)
    X <- .center(X, xmeans)
    z <- matW(X, y)
    lev <- z$lev
    nlev <- length(lev)
    W <- z$W * n / (n - nlev)
    ni <- z$ni
    z <- matB(X, y)
    B <- z$B
    ct <- z$ct
    if(is.null(nlv)) 
        nlv <- nlev - 1
    nlv <- min(nlv, p, nlev - 1)
    ## Temporary, as in rnirs:
    ## If W is singular, a MP pseudo-inverse is calculated 
    Winv <- tryCatch(solve(W), error = function(e) e)
    if(inherits(Winv, "error")) 
        Winv <- pinv(W)$Xplus
    ## End
    ## Winv %*% B is not symmetric
    fm <- eigen(Winv %*% B)
    P <- fm$vectors[, seq_len(nlv), drop = FALSE]
    eig <- fm$values
    P <- Re(P)
    eig <- Re(eig)
    sstot <- sum(eig)
    norm.P <- sqrt(diag(t(P) %*% W %*% P))
    P <- .scale(P, scale = norm.P)
    dimnames(P) <- list(colnames(W), paste("lv", seq_len(nlv), sep = ""))
    T <- X %*% P
    Tcenters <- ct %*% P
    structure(
        list(T = T, P = P, Tcenters = Tcenters, eig = eig, sstot = sstot,
            W = W, xmeans = xmeans, lev = lev, ni = ni),
        class = "Fda")
}

fdasvd <- function(X, y, nlv = NULL) {
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    if(is.factor(y))
        y <- as.character(y)
    xmeans <- colMeans(X)
    X <- .center(X, xmeans)
    z <- matW(X, y)
    lev <- z$lev    
    nlev <- length(lev)
    W <- z$W * n / (n - nlev)
    ni <- z$ni
    z <- matB(X, y)
    ct <- z$ct
    if(is.null(nlv)) 
        nlv <- nlev - 1
    nlv <- min(nlv, p, nlev - 1)
    ## Temporary, as in rnirs:
    ## If W is singular, a MP pseudo-inverse is calculated 
    Winv <- tryCatch(solve(W), error = function(e) e)
    if(inherits(Winv, "error")) 
        Winv <- pinv(W)$Xplus
    ## End
    U <- chol(Winv)
    tU <- t(U)
    Zct <- ct %*% tU
    zfm <- pcasvd(Zct, nlv = nlev - 1, weights = ni)
    Pz <- zfm$P
    Tcenters <- Zct %*% Pz        
    eig <- zfm$eig
    sstot <- zfm$sstot
    P <- tU %*% Pz[, seq_len(nlv), drop = FALSE]
    dimnames(P) <- list(colnames(W), paste("lv", seq_len(nlv), sep = ""))
    T <- X %*% P
    Tcenters <- ct %*% P
    structure(
        list(T = T, P = P, Tcenters = Tcenters, eig = eig, sstot = sstot,
            W = W, xmeans = xmeans, lev = lev, ni = ni),
        class = "Fda")
}

transform.Fda <- function(object, X, ..., nlv = NULL) {
    a <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- a
    else 
        nlv <- min(nlv, a)
    X <- .center(.mat(X), object$xmeans)
    T <- X %*% object$P
    T
}

summary.Fda <- function(object, ...) {
    nlv <- dim(object$T)[2]
    eig <- object$eig[seq_len(nlv)]
    pvar <-  eig / sum(object$eig)
    cumpvar <- cumsum(pvar)    
    explvar <- data.frame(lv = seq_len(nlv), var = eig, 
                          pvar = pvar, cumpvar = cumpvar)
    list(explvar = explvar)    
}



