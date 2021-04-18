kpca <- function(X, nlv, kern = "krbf", weights = NULL, ...) {
  
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
    Kc <- t(t(K - colSums(weights * tK)) - colSums(weights * tK)) + 
        sum(weights * t(weights * tK))
    
    fm <- eigen(sqrt(weights) * t(sqrt(weights) * t(Kc)))
    A <- fm$vectors[, seq_len(nlv)]
    eig <- fm$values[seq_len(nlv)]
    sv <- sqrt(eig)
    sstot <- sum(fm$values)
    
    P <- sqrt(weights) * .scale(A, scale = sv)
    T <- Kc %*% P
    colnames(T) <- colnames(P) <- paste("pc", seq_len(nlv), sep = "")
  
    structure(
        list(X = X, tK = tK, T = T, P = P, eig = eig, sv = sv, sstot = sstot,
            weights = weights,
            kern = kern, dots = dots),
        class = c("Kpca")
        )

    }

transform.Kpca <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    a <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- a
    else 
        nlv <- min(nlv, a)
    weights <- object$weights
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    Kc <- t(t(K - colSums(weights * t(K))) - colSums(weights * object$tK)) + 
        sum(weights * t(weights * object$tK))
    T <- Kc %*% object$P[, seq_len(nlv), drop = FALSE]
    T       
    }

summary.Kpca <- function(object, X = NULL, ...)
    summary.Pca(object, X, ...)
  

