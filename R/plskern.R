plskern <- function(X, Y, nlv, weights = NULL) {
    
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
    
    Xd <- weights * X
    # = D %*% X = d * X = X * d

    tXY <- crossprod(Xd, Y)
    # = t(D %*% X) %*% Y = t(X) %*% D %*% Y
    
    for(a in seq_len(nlv)) {
    
        if(q == 1) w <- tXY
            else {
                u <- svd(t(tXY), nu = 1, nv = 0)$u
                ## Same as
                ## u <- svd(tXY, nu = 0, nv = 1)$v
                ## u <- eigen(crossprod(tXY), symmetric = TRUE)$vectors[, 1]
                w <- tXY %*% u
                } 

        w <- w / sqrt(sum(w * w))
        
        r <- w
        if(a > 1)
            for(j in seq_len(a - 1)) 
                    r <- r - sum(P[, j] * w) * R[, j]
        
        t <- X %*% r 
        
        tt <- sum(weights * t * t)         
        
        c <- crossprod(tXY, r) / tt
        
        p <- crossprod(Xd, t) / tt 
        
        tXY <- tXY - tcrossprod(p, c) * tt    
        
        T[, a] <- t
        P[, a] <- p
        W[, a] <- w
        R[, a] <- r
        C[, a] <- c
        
        TT[a] <- tt
        
        }

    structure(
        list(T = T, P = P, R = R, W = W, C = C, TT = TT,
             xmeans = xmeans, ymeans = ymeans, weights = weights, U = NULL),
        class = "PlsOrtho"
        )
    
    }

transform.PlsOrtho <- function(fm, X, ..., nlv = NULL) {
   
    A <- dim(fm$P)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- min(nlv, A)
    
    T <- .center(.mat(X), fm$xmeans) %*% fm$R[, seq_len(nlv), drop = FALSE]
    colnames(T) <- paste("lv", seq_len(dim(T)[2]), sep = "")
    T
    
    }

coef.PlsOrtho <- function(fm, ..., nlv = NULL) {
  
    ## Works also for nlv = 0
  
    A <- dim(fm$P)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- min(nlv, A)
  
    beta <- t(fm$C)[seq_len(nlv), , drop = FALSE]
    B <- fm$R[, seq_len(nlv), drop = FALSE] %*% beta
    int <- fm$ymeans - t(fm$xmeans) %*% B
    
    list(int = int, B = B) 
  
    }

predict.PlsOrtho <- function(fm, X, ..., nlv = NULL) {
    
    X <- .mat(X)
    q <- dim(fm$C)[1]
    rownam <- row.names(X)
    colnam <- paste("y", seq_len(q), sep = "")
    
    A <- dim(fm$P)[2]
    if(is.null(nlv))
        nlv <- A 
    else 
        nlv <- seq(min(nlv), min(max(nlv), A))
    le_nlv <- length(nlv)
    
    pred <- vector(mode = "list", length = le_nlv)
    for(i in seq_len(le_nlv)) {
        z <- coef(fm, nlv = nlv[i])
        zpred <- t(c(z$int) + t(X %*% z$B))
        ## Same but faster than:
        ## zpred <- cbind(rep(1, m), X) %*% rbind(z$int, z$B)
        #zpred <- data.frame(zpred)
        dimnames(zpred) <- list(rownam, colnam)
        pred[[i]] <- zpred
        }
    names(pred) <- paste("lv", nlv, sep = "")
    if(le_nlv == 1)
        pred <- pred[[1]] 
    
    list(pred = pred)
    
    }
