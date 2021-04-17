krr <- function(X, Y, lb = 0, kern = "krbf", weights = NULL, ...) {
    
    X <- .mat(X, "x")
    Y <- .mat(Y, "y")     
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    q <- dim(Y)[2]
    
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    
    ymeans <- .colmeans(Y, weights = weights) 
    
    kern <- eval(parse(text = kern))
    dots <- list(...)
    K <- kern(X, ...)
    ## If non-symmetric Gram matrix
    tK <- t(K)     
    Kc <- t(t(K - colSums(weights * tK)) - colSums(weights * tK)) + 
        sum(weights * t(weights * tK))
    Kd <- sqrt(weights) * t(sqrt(weights) * t(Kc))
    
    fm <- eigen(Kd)
    tol <- sqrt(.Machine$double.eps)
    posit <- fm$values > max(tol * fm$values[1L], 0)
    
    A <- fm$vectors[, posit, drop = FALSE]
    eig <- fm$values[posit]
    sv <- sqrt(eig)
    
    P <- sqrt(weights) * .scale(A, scale = sv)
    T <- Kc %*% P
    tTDY <- crossprod(T, weights * Y)
    
    structure(
        list(
            X = X, K = K, tK = tK, 
            A = A, P = P, tTDY = tTDY, eig = eig, lb = lb, 
            ymeans = ymeans, weights = weights,
            kern = kern, dots = dots),
        class = c("Krr")
        )

    }

coef.Krr <- function(object, ..., lb = NULL) {
  
    n <- length(object$weights)
    if(is.null(lb))
        lb <- object$lb
    
    z <- 1 / (object$eig + lb / n)
    beta <- z * object$tTDY
    int <- object$ymeans
    tr <- sum(object$eig * z)

    list(int = int, beta = beta, df = 1 + tr) 
  
    }


predict.Krr <- function(object, X, ..., lb = NULL) {
    
    X <- .mat(X)
    q <- length(object$ymeans)
    rownam <- row.names(X)
    colnam <- paste("y", seq_len(q), sep = "")
    weights <- object$weights
    
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    Kc <- t(t(K - colSums(weights * t(K))) - colSums(weights * object$tK)) + 
        sum(weights * t(weights * object$tK))
    T <- Kc %*% object$P    

    if(is.null(lb))
        lb <- object$lb
    le_lb <- length(lb)
    
    pred <- vector(mode = "list", length = le_lb)
    for(i in seq_len(le_lb)) {
        z <- coef(object, lb = lb[i])
        zpred <- t(c(z$int) + t(T %*% z$beta))
        pred[[i]] <- zpred
        }
    names(pred) <- paste("lb", lb, sep = "")
    if(le_lb == 1)
        pred <- pred[[1]] 
    
    list(pred = pred)

    }
    
    