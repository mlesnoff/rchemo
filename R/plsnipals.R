plsnipals <- function(X, Y, nlv, weights = NULL) {
    
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    zp <- zdim[2]
    
    Y <- .mat(Y, "y")     
    q <- dim(Y)[2]
    
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    
    xmeans <- .colmeans(X, weights = weights) 
    X <- .center(X, xmeans)

    ymeans <- .colmeans(Y, weights = weights) 
    Y <- .center(Y, ymeans)
    
    nam <- paste("lv", seq_len(nlv), sep = "")
    T <- matrix(nrow = n, ncol = nlv, dimnames = list(row.names(X), nam))                     
    R <- W <- P <- matrix(nrow = zp, ncol = nlv, dimnames = list(colnames(X), nam)) 
    C <- matrix(nrow = q, ncol = nlv, dimnames = list(colnames(Y), nam))                     
    TT <- vector(length = nlv)
    
    for(a in seq_len(nlv)) {
            
        tXY <- crossprod(weights * X, Y)
        # = t(D %*% X) %*% Y = t(X) %*% D %*% Y
        
        if(q == 1) {
            w <- tXY
            w <- w / sqrt(sum(w * w))
            }
        else {
            w <- svd(tXY, nu = 1, nv = 0)$u
            }
        
        t <- X %*% w
        
        tt <- sum(weights * t * t)                                
        
        c <- crossprod(weights * Y, t)    / tt
        
        p <- crossprod(weights * X, t) / tt
        
        X <- X - tcrossprod(t, p)
        
        Y <- Y - tcrossprod(t, c)
        
        T[, a] <- t
        W[, a] <- w
        P[, a] <- p
        C[, a] <- c
        
        TT[a] <- tt

        }
    
    R <- W %*% solve(crossprod(P, W))

    structure(
        list(T = T, P = P, R = R, W = W, C = C, TT = TT,
             xmeans = xmeans, ymeans = ymeans, weights = weights, U = NULL),
        class = c("Plsortho", "Pls")
        )

    }
