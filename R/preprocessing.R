dderiv <- function(X, n = 5, ts = 1) {

    if(n < 3)
        stop("n must be higher or equal to 3.")
    if(n %% 2 == 0)
        stop("n must be an odd integer.")
    
    X <- .mat(X)
    p <- dim(X)[2]
    colnam <- colnames(X)

    zX <- X[, seq(n, p), drop = FALSE]
    X <- (zX - X[, seq_len(dim(zX)[2]), drop = FALSE]) / ts 
    
    u <- n - floor(n / 2)
    colnames(X) <- colnam[seq(u, (u + dim(X)[2] - 1))]
    
    X

    }

detrend <- function(X, degree = 1) {
  
    X <- .mat(X)
    dimnam <- dimnames(X)
  
    y <- seq_len(dim(X)[2])
    fun <- function(x, y, degree) 
        resid(lm(x ~ stats::poly(y, degree = degree)))
    
    tX <- t(X)
    X <- t(apply(tX, MARGIN = 2, FUN = fun, y = y, degree = degree))
  
    dimnames(X) <- dimnam
    X
  
    }

interpl <- function(X, w, meth = "cubic", ...) {
    
    X <- .mat(X)
    p <- dim(X)[2]
    
    colnam <- suppressWarnings(as.numeric(colnames(X)))
    if(sum(is.na(colnam)) > 0) 
        colnam <- seq_len(p)
    w0 <- colnam
    
    fun <- function(x, w0, w, method, ...)
        signal::interp1(w0, x, w, method, ...)
    zX <- t(apply(X, FUN = fun, MARGIN = 1, w0 = w0, w = w, method = meth))
    colnames(zX) <- w
    
    zX
    
    }

mavg <- function(X, n = 5) {
    
    if(n < 3) 
        stop("n must be higher or equal to 3.")
    if(n %% 2 == 0) 
        stop("n must be an odd integer.")
    
    X <- .mat(X)
    dimnam <- dimnames(X)
    
    fun <- function(x, n){
        x <- stats::filter(
            x, 
            filter = rep(1 / n, n), 
            method = "convolution", 
            sides = 2
            )
        as.vector(x)
        }
    
    tX <- t(X)
    X <- t(apply(tX, MARGIN = 2, FUN = fun, n = n))
    dimnames(X) <- dimnam
    
    s <- which(!is.na(X[1, ]))
    X <- X[, s, drop = FALSE]
    
    X

    }

savgol <- function(X, m, n, p, ts = 1) {
    
    X <- .mat(X)
    dimnam <- dimnames(X)
    
    fun <- signal::sgolayfilt
    
    tX <- t(X)
    X <- t(apply(tX, MARGIN = 2, FUN = fun, n = n, p = p, m = m, ts = ts))

    dimnames(X) <- dimnam
    
    X
    
    }

snv <- function(X, center = TRUE, scale = TRUE) {

    X <- t(.mat(X))
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    if(center) 
        xmeans <- colMeans(X)
    else 
        xmeans <- rep(0, p)
    
    if(scale) 
        xscales <- sqrt(.colvars(X)) 
    else 
        xscales <- rep(1, p)
    
    X <- t(.scale(X, xmeans, xscales))
    
    X
    
    }

