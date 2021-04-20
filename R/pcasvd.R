pcasvd <- function(X, nlv, weights = NULL) {
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
    res <- svd(sqrt(weights) * X, nu = 0, nv = nlv)
    P <- res$v
    sv <- res$d[seq_len(min(n, p))]
    sv[sv < 0] <- 0
    T <- X %*% P
    ## If weights > 0
    ## = 1 / sqrt(weights) * res$u %*% diag(sv, nrow = nlv)
    eig <- sv^2         
    ## eig
    ## = eigenvalues of X'DX = Cov(X) in metric D 
    ## = variances of scores T in metric D
    ## = TT = colSums(weights * T * T)  
    ## = norms^2 of the scores T in metric D
    ## = .xnorm(T, weights = weights)^2
    ## In general: sstot = weights * X * X =  sum(eig), 
    ## but not in NIPALS 
    sstot <- sum(eig)    
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
    colnames(T) <- colnames(P) <- paste("pc", seq_len(nlv), sep = "")
    structure(
        list(T = T, P = P, eig = eig, sv = sv, sstot = sstot,
            xmeans = xmeans, weights = weights, niter = NULL, conv = NULL),
        class = c("Pca")
        )
    }

transform <- function(object, X, ...) UseMethod("transform")

transform.Pca <- function(object, X, ..., nlv = NULL) {
    a <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- a
    else 
        nlv <- min(nlv, a)
    T <- .center(.mat(X), 
                 object$xmeans) %*% object$P[, seq_len(nlv), drop = FALSE]
    colnames(T) <- paste("pc", seq_len(dim(T)[2]), sep = "")
    T
    }

summary.Pca <- function(object, X = NULL, ...) {
    nlv <- dim(object$T)[2]
    TT <- object$weights * object$T * object$T
    tt <- colSums(TT)
    pvar <- tt / object$sstot
    cumpvar <- cumsum(pvar)
    explvar <- data.frame(pc = seq(nlv), var = tt, pvar = pvar, cumpvar = cumpvar)
    row.names(explvar) <- seq(nlv)
    contr.ind <- data.frame(.scale(TT, center = rep(0, nlv), scale = tt))
    cor.circle <- contr.var <- coord.var <- NULL
    if(!is.null(X)) {
        p <- dim(X)[2]
        xvars <- .colvars(X, weights = object$weights)
        zX <- .scale(X, center = rep(0, p), scale = sqrt(xvars))
        zT <- .scale(object$T, center = rep(0, nlv), scale = sqrt(tt))
        cor.circle <- data.frame(t(object$weights * zX) %*% zT)
        coord.var <- data.frame(crossprod(X, object$weights * zT))
        z <- coord.var^2
        contr.var <- data.frame(.scale(z, rep(0, nlv), colSums(z)))
        row.names(cor.circle) <- row.names(contr.var) <- row.names(coord.var) <- row.names(object$P)
        }
    list(explvar = explvar, contr.ind = contr.ind, 
        contr.var = contr.var, coord.var = coord.var, cor.circle = cor.circle)    
    }


