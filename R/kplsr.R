kplsr <- function(X, Y, weights = NULL, nlv, kern = "krbf",
     tol = .Machine$double.eps^0.5, maxit = 100, ...) {
    X <- .mat(X)
    Y <- .mat(Y, "y")     
    zdim <- dim(X)
    n <- zdim[1]
    zp <- zdim[2]
    q <- dim(Y)[2]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    ymeans <- .colmeans(Y, weights = weights) 
    Y <- .center(Y, ymeans)
    kern <- eval(parse(text = kern))
    dots <- list(...)
    K <- kern(X, ...)
    ## If non-symmetric Gram matrix
    Kt <- t(K)
    Kc <- K <- t(t(K - colSums(weights * Kt)) - colSums(weights * Kt)) + 
        sum(weights * t(weights * Kt))
    nam <- paste("lv", seq_len(nlv), sep = "")
    U <- T <- matrix(nrow = n, ncol = nlv, dimnames = list(row.names(X), nam))                     
    C <- matrix(nrow = q, ncol = nlv, dimnames = list(colnames(Y), nam)) 
    for(a in seq_len(nlv)) {
        if(q == 1) {
            t <- K %*% (weights * Y)
            t <- t / sqrt(sum(weights * t * t))
            c <- crossprod(weights * Y, t)
            u <- Y %*% c
            u <- u / sqrt(sum(u * u))
        }
        else {
            u <- Y[, 1]
            ztol <- 1
            iter <- 1
            while(ztol > tol & iter <= maxit) {
                t <- K %*% (weights * u)
                t <- t / sqrt(sum(weights * t * t))
                c <- crossprod(weights * Y, t)
                zu <- Y %*% c
                zu <- zu / sqrt(sum(zu * zu))
                ztol <- .colnorms(u - zu)
                u <- zu
                iter <- iter + 1
            }
        }
        z <- diag(n) - tcrossprod(t, weights * t)
        ## Slow, ~ .2 s/component for n=500 ==> 4 s for nlv=20
        K <- z %*% K %*% t(z)
        ## End
        Y <- Y - tcrossprod(t, c)
        T[, a] <- t
        C[, a] <- c
        U[, a] <- u
    }
    DU <- weights * U
    zR <- DU %*% solve(crossprod(T, weights * Kc) %*% DU)
    structure(
        list(X = X, Kt = Kt, T = T, C = C, U = U, R = zR,
             ymeans = ymeans, weights = weights,
             kern = kern, dots = dots),
        class = c("Kplsr", "Kpls"))
    }

transform.Kplsr <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    a <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- a
    else 
        nlv <- min(nlv, a)
    weights <- object$weights
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    Kc <- t(t(K - colSums(weights * t(K))) - colSums(weights * object$Kt)) + 
        sum(weights * t(weights * object$Kt))
    T <- Kc %*% object$R[, seq_len(nlv), drop = FALSE]
    T
    }

coef.Kplsr <- function(object, ..., nlv = NULL) {
    a <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- a
    else 
        nlv <- min(a, nlv)
    beta <- t(object$C)[seq_len(nlv), , drop = FALSE]
    int <- object$ymeans
    list(int = int, beta = beta) 
    }

predict.Kplsr <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    q <- length(object$ymeans)
    rownam <- row.names(X)
    colnam <- paste("y", seq_len(q), sep = "")
    weights <- object$weights
    a <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- a 
    else 
        nlv <- seq(min(nlv), min(max(nlv), a))
    le_nlv <- length(nlv)
    T <- transform(object, X)
    pred <- vector(mode = "list", length = le_nlv)
    for(i in seq_len(le_nlv)) {
        z <- coef(object, nlv = nlv[i])
        zpred <- t(c(z$int) + t(T[, seq_len(nlv[i]), drop = FALSE] %*% z$beta))
        pred[[i]] <- zpred
        }
    names(pred) <- paste("nlv", nlv, sep = "")
    if(le_nlv == 1)
        pred <- pred[[1]] 
    list(pred = pred)
    }





