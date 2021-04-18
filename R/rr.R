rr <- function(X, Y, lb = 0, weights = NULL) {
    
    X <- .mat(X, "x")
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    
    Y <- .mat(Y, "y")     
    q <- dim(Y)[2]
    
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    
    xmeans <- .colmeans(X, weights = weights) 
    X <- .center(X, xmeans)
    ymeans <- .colmeans(Y, weights = weights) 

    tol <- sqrt(.Machine$double.eps) 
    if(n >= p) {
        fm <- eigen(crossprod(sqrt(weights) * X), symmetric = TRUE)
        posit <- fm$values > max(tol * fm$values[1L], 0)
        eig <- fm$values[posit]
        V <- fm$vectors[, posit, drop = FALSE]
        }
    else {
        zX <- sqrt(weights) * X
        fm <- eigen(tcrossprod(zX), symmetric = TRUE)
        posit <- fm$values > max(tol * fm$values[1L], 0)
        eig <- fm$values[posit]
        U <- fm$vectors[, posit, drop = FALSE]
        V <- crossprod(zX, .scale(U, scale = sqrt(eig)))
        } 
    T <- X %*% V
    tTDY <- crossprod(T, weights * Y)
    
    structure(
        list(
            V = V, tTDY = tTDY, eig = eig, lb = lb, 
            xmeans = xmeans, ymeans = ymeans, weights = weights),
        class = c("Rr")
        )

    }

coef.Rr <- function(object, ..., lb = NULL) {
    n <- length(object$weights)
    if(is.null(lb))
        lb <- object$lb
    z <- 1 / (object$eig + lb / n)
    beta <- z * object$tTDY
    B <- object$V %*% beta
    int <- object$ymeans - crossprod(object$xmeans, B)
    tr <- sum(object$eig * z)
    row.names(B) <- paste("x", seq_len(dim(B)[1]), sep = "")
    list(int = int, B = B, df = 1 + tr) 
    }

predict.Rr <- function(object, X, ..., lb = NULL) {
    X <- .mat(X)
    q <- length(object$ymeans)
    rownam <- row.names(X)
    colnam <- paste("y", seq_len(q), sep = "")
    if(is.null(lb))
        lb <- object$lb
    le_lb <- length(lb)
    pred <- vector(mode = "list", length = le_lb)
    for(i in seq_len(le_lb)) {
        z <- coef(object, lb = lb[i])
        zpred <- t(c(z$int) + t(X %*% z$B))
        dimnames(zpred) <- list(rownam, colnam)
        pred[[i]] <- zpred
        }
    names(pred) <- paste("lb", lb, sep = "")
    if(le_lb == 1)
        pred <- pred[[1]] 
    list(pred = pred)
    }
    
    
    
    
    

