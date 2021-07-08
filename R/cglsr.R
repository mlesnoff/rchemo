cglsr <- function(X, y, nlv, reorth = TRUE, filt = FALSE) {
    X <- .mat(X)
    y <- .mat(y, "y")     
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    xmeans <- .colmeans(X) 
    X <- .center(X, xmeans)
    ymeans <- .colmeans(y) 
    y <- .center(y, ymeans)
    fudge <- 1e-4
    B <- matrix(nrow = p, ncol = nlv)
    b <- rep(0, p) 
    r <- y       # r = y - X * b, with b = 0
    zp <- s <- crossprod(X, r)
    g <- sum(s * s)
    gnew <- rep(0, nlv)
    if(reorth) {
        A <- matrix(nrow = p, ncol = nlv + 1)
        A[, 1] <- s / sqrt(g)
    }
    F <- NULL
    if(filt) {
        eig <- svd(X)$d^2
        if(n < p)
            eig <- c(eig, rep(0, p - n))
        F <- matrix(nrow = p, ncol = nlv)
        Fd <- rep(0, p)
    }
    for(j in seq_len(nlv)) {
        q <- X %*% zp
        alpha <- g / sum(q * q)
        b <- b + alpha * zp
        r <- r - alpha * q
        s <- crossprod(X, r)
        # Reorthogonalize s to previous s-vectors
        if (reorth) {
          for(i in seq_len(j))
              s <- s - sum(A[, i] * s) * A[, i]
              A[, j + 1] <- s / sqrt(sum(s * s))
        }
        # End
        gnew[j] = sum(s * s) 
        beta <- gnew[j] / g
        g <- gnew[j]
        zp <- s + beta * zp
        B[, j] <- b  
        # Filter factors
        # fudge threshold is used to prevent filter factors from exploding
        if(filt) {
            if(j == 1) {
                F[, 1] <- alpha * eig
                Fd <- eig - eig * F[, 1] + beta * eig
            }
            else {
                F[, j] <- F[, j - 1] + alpha * Fd
                Fd <- eig - eig * F[, j] + beta * Fd
            }
            if (j > 2) {
                u <- which(abs(F[, j - 1] - 1) < fudge & abs(F[, j - 2] - 1) < fudge)
                if(length(u) > 0)
                    F[u, j] <- rep(1, length(u))
            }
        }
        # End
    }
    structure(
        list(B = B, gnew = gnew, xmeans = xmeans, ymeans = ymeans, F = F),
        class = "Cglsr")
}

coef.Cglsr <- function(object, ..., nlv = NULL) {
    a <- dim(object$B)[2]
    if(is.null(nlv))
        nlv <- a
    else 
        nlv <- min(a, nlv)
    B <- object$B[, nlv, drop = FALSE]
    int <- object$ymeans - t(object$xmeans) %*% B
    list(int = int, B = B) 
}

predict.Cglsr <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    q <- length(object$ymeans)
    rownam <- row.names(X)
    colnam <- paste("y", seq_len(q), sep = "")
    a <- dim(object$B)[2]
    if(is.null(nlv))
        nlv <- a 
    else 
        nlv <- seq(min(nlv), min(max(nlv), a))
    le_nlv <- length(nlv)
    pred <- vector(mode = "list", length = le_nlv)
    for(i in seq_len(le_nlv)) {
        z <- coef(object, nlv = nlv[i])
        zpred <- t(c(z$int) + t(X %*% z$B))
        dimnames(zpred) <- list(rownam, colnam)
        pred[[i]] <- zpred
    }
    names(pred) <- paste("lv", nlv, sep = "")
    if(le_nlv == 1)
        pred <- pred[[1]] 
    list(pred = pred)
}

dfplsr_cg <- function(X, y, nlv, reorth = TRUE) {
    F <- cglsr(X, y, nlv = nlv, reorth = reorth, filt = TRUE)$F
    df <- c(1, 1 + colSums(F))
    list(df = df)
}



    
    
    
    
    
    