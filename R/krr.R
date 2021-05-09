krr <- function(X, Y, weights = NULL, lb = 1e-2, kern = "krbf", ...) {
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
    Kt <- t(K)     
    ## Kc = Phi * Phi', where Phi is centered
    Kc <- t(t(K - colSums(weights * Kt)) - colSums(weights * Kt)) + 
        sum(weights * t(weights * Kt))
    ## Kd = D^(1/2) * Kc * D^(1/2) 
    ## = U * Delta^2 * U'
    Kd <- sqrt(weights) * t(sqrt(weights) * t(Kc))
    fm <- svd(Kd, nu = 0)
    U <- fm$v
    sv <- sqrt(fm$d)
    ## U' * D^(1/2) * Y
    UtDY <- crossprod(U, sqrt(weights) * Y)
    structure(
        list(
            X = X, K = K, Kt = Kt, 
            U = U, UtDY = UtDY, sv = sv, lb = lb, 
            ymeans = ymeans, weights = weights,
            kern = kern, dots = dots),
        class = c("Krr"))
}

coef.Krr <- function(object, ..., lb = NULL) {
    n <- length(object$weights)
    eig <- object$sv^2
    if(is.null(lb))
        lb <- object$lb
    z <- 1 / (eig + lb^2)
    alpha <- object$U %*% (z * object$UtDY)
    int <- object$ymeans
    tr <- sum(eig * z)
    list(int = int, alpha = alpha, df = 1 + tr) 
}

predict.Krr <- function(object, X, ..., lb = NULL) {
    X <- .mat(X)
    q <- length(object$ymeans)
    rownam <- row.names(X)
    colnam <- paste("y", seq_len(q), sep = "")
    weights <- object$weights
    ## Kc = Knew
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    Kc <- t(t(K - colSums(weights * t(K))) - colSums(weights * object$Kt)) + 
        sum(weights * t(weights * object$Kt))
    if(is.null(lb))
        lb <- object$lb
    le_lb <- length(lb)
    pred <- vector(mode = "list", length = le_lb)
    sqrtw <- sqrt(object$weights)
    for(i in seq_len(le_lb)) {
        z <- coef(object, lb = lb[i])
        ## INT + Knew * D^(1/2) * alpha
        zpred <- t(c(z$int) + t(Kc %*% (sqrtw * z$alpha)))
        pred[[i]] <- zpred
    }
    names(pred) <- paste("lb", lb, sep = "")
    if(le_lb == 1)
        pred <- pred[[1]] 
    list(pred = pred)
}
    
    