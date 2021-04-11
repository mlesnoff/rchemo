summary.Pca <- function(object, X, ...) {
    
    X <- .center(X, object$xmeans)
    sstot <- sum(object$weights * X * X, na.rm = TRUE)    
    ## In general: sstot =  sum(eig)   
    TT <- object$weights * object$T * object$T
    tt <- colSums(TT)
    pvar <- tt / sstot
    cumpvar <- cumsum(pvar)
    znlv <- seq(dim(object$T)[2])
    explvar <- data.frame(pc = znlv, var = tt, pvar = pvar, cumpvar = cumpvar)
    row.names(explvar) <- znlv
    
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
    
summary.Pls <- function(object, X, ...) {
    
    zdim <- dim(object$T)
    n <- zdim[1]
    nlv <- zdim[2]
    X <- .center(X, object$xmeans)
    sstot <- sum(object$weights * X * X, na.rm = TRUE)    
    tt <- object$TT
    tt.adj <- colSums(object$P * object$P) * tt
    pvar <- tt.adj / sstot
    cumpvar <- cumsum(pvar)
    xvar <- tt.adj / n
    znlv <- seq(nlv)
    z <- data.frame(nlv = znlv, var = xvar, pvar = pvar, cumpvar = cumpvar)
    row.names(z) <- znlv
    list(explvarx = z)
    
    }





