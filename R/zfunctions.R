.center <- function(X, center = colMeans(X)) 
    t((t(X) - c(center)))

.colcors <- function(X, weights = NULL) {
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    xmeans <- .colmeans(X, weights = weights)
    xvars <- .colvars(X, weights = weights)
    X <- .scale(X, xmeans, sqrt(xvars))
    crossprod(sqrt(weights) * X)
    }

.colcors_xy <- function(X, Y, weights = NULL) {
    X <- .mat(X)
    n <- dim(X)[1]
    Y <- .mat(Y)
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    xmeans <- .colmeans(X, weights = weights)
    xvars <- .colvars(X, weights = weights)
    ymeans <- .colmeans(Y, weights = weights)
    yvars <- .colvars(Y, weights = weights)
    X <- .scale(X, xmeans, sqrt(xvars))
    Y <- .scale(Y, ymeans, sqrt(yvars))
    crossprod(weights * X, Y)
    }

.colcovs <- function(X, weights = NULL) {
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    xmeans <- .colmeans(X, weights = weights)
    X <- .center(X, xmeans)
    crossprod(sqrt(weights) * X)
    }

.colcovs_xy <- function(X, Y, weights = NULL) {
    X <- .mat(X)
    n <- dim(X)[1]
    Y <- .mat(Y)
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    xmeans <- .colmeans(X, weights = weights)
    ymeans <- .colmeans(Y, weights = weights)
    X <- .center(X, xmeans)
    Y <- .center(Y, ymeans)
    crossprod(weights * X, Y)
    }

.colmeans <- function(X, weights = NULL) {
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    colSums(weights * X)   
    }

.colmeds_spa <- function(X, delta = 1e-6) {
  
    X <- .mat(X)
  
    ##### COPY OF FUNCTION 'spatial.median' AVAILABLE IN THE SCRIPT PcaLocantore.R
    ##### OF PACKAGE rrcov v.1.4-3 on R CRAN (Thanks to V. Todorov, 2016)

    x <- X
  
    dime <- dim(x)
    n <- dime[1]
    p <- dime[2]
    delta1 <- delta * sqrt(p)
  
    mu0 <- apply(x, 2, median)
  
    h <- delta1 + 1
    tt <- 0
    while(h > delta1) {
        tt <- tt + 1
        TT <- matrix(mu0, n, p, byrow = TRUE)
        U <- (x - TT)^2
    
        w <- sqrt(rowSums(U))
    
        w0 <- median(w)
        ep <- delta*w0

        z <- (w <= ep)
        w[z] <- ep
        w[!z] <- 1 / w[!z]
        w <- w / sum(w)
        x1 <- x
        for(i in seq_len(n))
            x1[i, ] <- w[i] * x[i, ]
    
        mu <- colSums(x1)
    
        h <- sqrt(sum((mu - mu0)^2))
        mu0 <- mu
        }

    ##### END
  
    mu0

    }

.colnorms <- function(X, weights = NULL) {
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights)) 
        weights <- rep(1, n)
    weights <- c(weights)
    sqrt(colSums(weights * X * X))
    }

.colvars <- function(X, weights = NULL) {
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    xmeans <- .colmeans(X, weights = weights)
    X <- .center(X, xmeans)
    colSums(weights * X * X)  / sum(weights)
    } 

.eigpow <- function(X, tol = .Machine$double.eps^0.5, maxit = 30) {
    ztol <- 1
    iter <- 1
    v <- X[, which.max(.colvars(X))]
    while(ztol > tol & iter <= maxit) {
      zv <- X %*% v
      zv <- zv / sqrt(sum(zv * zv))
      ztol <- .colnorms(v - zv)
      v <- zv
      iter <- iter + 1
      }      
    list(v = c(v), niter = iter)
    }  

.ellips <- function(shape, center = rep(0, ncol(shape)), radius = 1) {
    # The generated ellipse is = (x - mu)' * S^(-1) * (x - mu) <= r^2  
    # shape = variance-covariance matrix S (size q x q) of x (vector of length q)
    # center = mu (vector of length q)
    # radius = r
    theta <- seq(0, 2 * pi, length = 51)
    circ <- radius * cbind(cos(theta), sin(theta))
    z <- eigen(shape)
    d <- sqrt(z$values)
    V <- z$vectors
    X <- V %*% diag(d) %*% t(circ)
    X <- t(X + center)
    list(X = X, V = V, d = d)
    }

.findmax <- function(x) {
    x <- which.max(x)
    le <- length(x)
    if(le > 1)
        x <- x[sample(seq_len(le), 1)]
    x
    }

## !!!! In rchemo, a vector (n,) is considered as
## a column matrix (n, 1)
.mat <- function(X, prefix = NULL) {
  
    if(is.vector(X)) 
        X <- matrix(X, ncol = 1)

    if(!is.matrix(X)) 
        X <- as.matrix(X)
  
    if(is.null(row.names(X))) 
        row.names(X) <- seq_len(dim(X)[1])
    
    if(is.null(prefix)) {
        if(is.null(colnames(X)))
            colnames(X) <- paste("x", seq_len(dim(X)[2]), sep = "")
        }
    else  
        colnames(X) <- paste(prefix, seq_len(dim(X)[2]), sep = "")
    
    X
  
    }

.mweights <- function(weights)
    weights <- c(weights) / sum(weights)

.replace_bylev <- function(x, lev) {
    ## Replaces the elements of x
    ## by the levels of corresponding rank
    ## x: Vector of integers between 1 and nlev
    ## lev: vector of length nlev containing the levels
    ## Before replacement, vector lev sorted
    lev <- sort(unique(lev))
    sapply(x, FUN = function(x) lev[x], simplify = TRUE)
    }

.scale = function(X, center = rep(0, dim(X)[2]), scale) 
    t((t(X) - c(center)) / c(scale))

