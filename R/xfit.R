xfit <- function(object, X, ...) UseMethod("xfit")

xfit.Pca <- xfit.Plsr <- function(object, X, ..., nlv = NULL) {
    A <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- min(nlv, A)
    if(nlv == 0) {
        m <- dim(X)[1]
        z <- rep(object$xmeans, m)
        X <- .mat(matrix(z, nrow = m, byrow = TRUE))
        } 
    else {
        X <- .center(
            tcrossprod(transform(object, X, nlv = nlv), object$P[, 1:nlv, drop = FALSE]),
            -object$xmeans
            )  
        }
    X
    }

xresid <- function(object, X, ..., nlv = NULL) {
    A <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- min(nlv, A)   
    if(nlv == 0)
        E <- .center(X, object$xmeans)
    else 
        E <- X - xfit(object, X, nlv = nlv)
    E
    }




