rr <- function(X, Y, weights = NULL, lb = 1e-2) {
    X <- .mat(X, "x")
    Y <- .mat(Y, "y")     
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    q <- dim(Y)[2]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    xmeans <- .colmeans(X, weights = weights) 
    X <- .center(X, xmeans)
    ymeans <- .colmeans(Y, weights = weights) 
    fm <- svd(sqrt(weights) * X)
    U <- fm$u
    V <- fm$v
    sv <- fm$d
    ## T = X V
    ## T'DY = Delta * U' * D^(1/2) * Y
    TtDY <- sv * t(U) %*% (sqrt(weights) * Y)
    structure(
        list(V = V, TtDY = TtDY, sv = sv, lb = lb, 
            xmeans = xmeans, ymeans = ymeans, weights = weights),
        class = c("Rr")
        )
    }

coef.Rr <- function(object, ..., lb = NULL) {
    n <- length(object$weights)
    eig <- object$sv^2
    if(is.null(lb))
        lb <- object$lb
    z <- 1 / (eig + lb^2)
    beta <- z * object$TtDY
    B <- object$V %*% beta
    int <- object$ymeans - crossprod(object$xmeans, B)
    tr <- sum(eig * z)
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
    
    
    
    
    

