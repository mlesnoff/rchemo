pcanipalsna <- function(
    X, 
    nlv, 
    gs = TRUE, 
    tol = .Machine$double.eps^0.5, maxit = 200) {
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    nbcol <- zdim[2]
    nlv <- min(nlv, n, nbcol)
    xmeans <- colMeans(X, na.rm = TRUE)
    X <- .center(X, xmeans)
    sv <- tt <- vector(length = nlv)
    T <- matrix(nrow = n, ncol = nlv)
    P <- matrix(nrow = nbcol, ncol = nlv)
    s <- which(is.na(X))
    if(length(s) > 0) {
        isna <- TRUE
        ts <- which(is.na(t(X)))
    }
    else
        isna <- FALSE
    niter <- vector(length = nlv)
    PtP <- matrix(0, nrow = nbcol, ncol = nbcol)
    TtT <- matrix(0, nrow = n, ncol = n)
    for(a in seq_len(nlv)) {
        j <- which.max(colSums(abs(X), na.rm = TRUE))
        if(isna) {
            X0 <- replace(X, s, 0)
            t <- X0[, j]
        }
        else
            t <- X[, j]
        iter <- 1
        cont <- TRUE
        while(cont) {
            ## Regression of X on t
            if(isna) {
                zTT <- replace(
                    matrix(t * t, nrow = n, ncol = nbcol), 
                    s, 0
                    )
                p <- crossprod(X0, t) / colSums(zTT)
            }
            else
                p <- crossprod(X, t) / sum(t * t)
            if(gs & a > 1)
                p <- p - PtP %*% p
            p <- p / sqrt(sum(p * p))
            zt <- t
            ## Regression of X' on p
            if(isna) {
                zPP <- replace(
                    matrix(p * p, nrow = nbcol, ncol = n), 
                    ts, 0
                    )
                t = X0 %*% p / colSums(zPP)
            }
            else
                t <- X %*% p
            if(gs & a > 1) 
                t <- t - TtT %*% t
            ztol <- sum((t - zt)^2)
            iter <- iter + 1
            if(ztol < tol | iter > maxit)
                cont <- FALSE
        }
        X <- X - tcrossprod(t, p)
        P[, a] <- p
        T[, a] <- t
        tt[a] <- sum(t * t)
        sv[a] <- sqrt(tt[a] / n)
        if(gs) {
            PtP <- PtP + tcrossprod(p)
            TtT <- TtT + tcrossprod(t) / tt[a]
        }
        niter[a] <- iter - 1
    }
    eig <- sv^2
    conv <- ifelse(niter < maxit, TRUE, FALSE)
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
    colnames(P) <- colnames(T) <- paste("comp", seq_len(nlv), sep = "")
    weights <- rep(1 / n, n)
    structure(
        list(T = T, P = P, sv = sv, eig = eig,
            xmeans = xmeans, weights = weights, niter = niter, conv = conv),
        class = c("Pca"))  
}

    
