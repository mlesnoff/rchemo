krbf <- function(X, Y = NULL, gamma = 1)
    exp(-gamma * euclsq(X, Y))

kpol <- function(X, Y = NULL, degree = 1, gamma = 1, coef0 = 0) {
    if(is.null(Y))
        K <- (gamma * tcrossprod(X) + coef0)
    else
        K <- (gamma * tcrossprod(X, Y) + coef0)
    if(degree > 1) {
        zK <- K
        for(i in seq_len(degree - 1))
            K <- K * zK
        }
    K    
    }

ktanh <- function(X, Y = NULL, gamma = 1, coef0 = 0)
    tanh(kpol(X, Y, degree = 1, gamma, coef0))

#kgram <- function(Xtrain, X = NULL, kern = krbf, ...) {
#    Xtrain <- .mat(Xtrain)
#    Ktrain <- kern(Xtrain, ...)
#    K <- NULL
#    if(!is.null(X))
#        K <- kern(.mat(X), Xtrain, ...)
#    list(Ktrain = Ktrain, K = K)
#    }















