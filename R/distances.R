euclsq <- function(X, Y = NULL) {
    ## Squared Euclidean distances
    ## between the rows of X, or between the rows of X and Y
    ## X = (n, p)  ;  Y = (m, p)
    ## euclsq(X)    ==> matrix (n, n) 
    ## euclsq(X, Y) ==> matrix (n, m) 
    X <- .mat(X)
    if(is.null(Y)) {
        sq <- rowSums(X * X)
        pmax(outer(sq, sq, "+") - 2 * tcrossprod(X), 0)
        }
    else {
        Y <- .mat(Y)
        sqx <- rowSums(X * X)
        sqy <- rowSums(Y * Y)
        pmax(outer(sqx, sqy, "+") - 2 * tcrossprod(X, Y), 0)
        }
    }

euclsq_mu <- function(X, mu) {
    ## Squared Euclidean distances
    ## between the rows of X and vector mu
    ## X = (n, p) ==> matrix (n, 1) 
    X <- .center(.mat(X), mu)
    dimnames <- list(row.names(X), "1")
    matrix(rowSums(X * X), ncol = 1, dimnames = dimnames)
    }

mahsq <- function(X, Y = NULL, U = NULL) {
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(U)) {
        S <- cov(X) * (n - 1) / n
        U <- chol(S)
        }
    else 
        U <- as.matrix(U)
    Uinv <- solve(U)
    X <- X %*% Uinv
    if(is.null(Y))
        D <- euclsq(X)
    else {
        Y <- .mat(Y) %*% Uinv
        D <- euclsq(X, Y)
        }
    D
    }

mahsq_mu <- function(X, mu, U = NULL) {
    X <- .mat(X)
    if(is.null(U)) {
        n <- dim(X)[1]
        S <- cov(X) * (n - 1) / n
        U <- chol(S)
        }
    else 
        U <- as.matrix(U)
    Uinv <- solve(U)
    zX <- X %*% Uinv
    zmu <- mu %*% Uinv
    euclsq_mu(zX, zmu)
    }


