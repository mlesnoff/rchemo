covsel <- function(X, Y, nvar = NULL, scaly = TRUE, weights = NULL) {
  
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    Y <- .mat(Y, "x")
    q <- dim(Y)[2]
  
    if(is.null(nvar)) 
        nvar <- p
  
    if(is.null(weights))
        weights <- rep(1 / n, n)
    weights <- .mweights(weights)
  
    xmeans <- .colmeans(X, weights = weights)
    X <- .center(X, xmeans)
    ymeans <- .colmeans(Y, weights = weights)
    Y <- .center(Y, ymeans)
  
    if(scaly)
        Y <- .scale(Y, rep(0, q), sqrt(colSums(weights * Y * Y)))
  
    xsstot <- sum(weights * X * X)
    ysstot <- sum(weights * Y * Y)
    yss <- xss <- selvar <- vector(length = nvar)
    for(i in seq_len(nvar)) {
        z <- rowSums(crossprod(weights * X, Y)^2)
        selvar[i] <- which(z == max(z))
        u <- X[, selvar[i], drop = FALSE]
        P <- tcrossprod(u) %*% diag(weights) / sum(weights * u * u)
        # Same as
        #P <- u %*% solve(t(u) %*% D %*% u) %*% t(u) %*% D
        #P <- u %*% t(u) %*% D / sum(d * u * u)
        #P <- crossprod(u %*% t(u), D) / sum(d * u * u)
        ## End
        ## The deflated X and Y are centered matrix (with metric D)
        X <- X - P %*% X 
        Y <- Y - P %*% Y
        xss[i] <- sum(weights * X * X)
        yss[i] <- sum(weights * Y * Y)
        }
    cumpvarx <- 1 - xss / xsstot
    cumpvary <- 1 - yss / ysstot
    sel <- data.frame(sel = selvar, 
                      cumpvarx = cumpvarx, cumpvary = cumpvary)
  
    list(sel = sel, weights = weights)

    }

