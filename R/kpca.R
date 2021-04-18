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
    xsstot <- sum(fm$values)
    
    P <- sqrt(weights) * .scale(A, scale = sv)
    T <- Kc %*% P
    colnames(T) <- colnames(P) <- paste("pc", seq_len(nlv), sep = "")
  
    structure(
        list(X = X, P = P, T = T, P = P, sv = sv, eig = eig,
            weights = weights,
            kern = kern, dots = dots),
        class = c("Kpca")
        )

    }

transform.Kpca <- function(object, X, ..., nlv = NULL) {
  
  
          Ku <- kern(Xu, Xr, ...)
        Kuc <- t(t(Ku - colSums(weights * t(Ku))) - colSums(weights * tK)) + 
            sum(weights * t(weights * tK))

        Tu <- Kuc %*% Pr
        
        row.names(Tu) <- row.names(Xu)
        colnames(Tu) <-    paste("comp", seq_len(ncomp), sep = "")

  
  
  
    A <- dim(object$P)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- min(nlv, A)
    T <- .center(.mat(X), 
                 object$xmeans) %*% object$P[, seq_len(nlv), drop = FALSE]
    colnames(T) <- paste("pc", seq_len(dim(T)[2]), sep = "")
    T
    }

summary.Pca <- function(object, X, ...) {
    
    p <- dim(X)[2]
    nlv <- dim(object$T)[2]
    
    X <- .center(X, object$xmeans)
    sstot <- sum(object$weights * X * X, na.rm = TRUE)    
    ## In general: sstot =  sum(eig)   
    TT <- object$weights * object$T * object$T
    tt <- colSums(TT)
    pvar <- tt / sstot
    cumpvar <- cumsum(pvar)
    explvar <- data.frame(pc = seq(nlv), var = tt, pvar = pvar, cumpvar = cumpvar)
    row.names(explvar) <- seq(nlv)
    
    contr.ind <- data.frame(.scale(TT, center = rep(0, nlv), scale = tt))
    
    xvars <- .colvars(X, weights = object$weights)
    zX <- .scale(X, center = rep(0, p), scale = sqrt(xvars))
    zT <- .scale(object$T, center = rep(0, nlv), scale = sqrt(tt))
    cor.circle <- data.frame(t(object$weights * zX) %*% zT)
    coord.var <- data.frame(crossprod(X, object$weights * zT))
    z <- coord.var^2
    contr.var <- data.frame(.scale(z, rep(0, nlv), colSums(z)))
    
    list(explvar = explvar, contr.ind = contr.ind, 
        contr.var = contr.var, coord.var = coord.var, cor.circle = cor.circle)    

    }



