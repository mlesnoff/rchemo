aggmean <- function(X, y = NULL) {
    
    ## Works also if y is a factor
    
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    colnam <- colnames(X)
    
    if(is.null(y)) 
        y <- rep(1, n)

    lev <-  sort(unique(y))
    nlev <- length(lev)
    ni <- c(table(y))
    
    ct <- matrix(nrow = nlev, ncol = p)
    for(i in seq_len(nlev))
        ct[i, ] <- colMeans(X[y == lev[i], , drop = FALSE])
    dimnames(ct) <- list(lev, colnam)

    list(ct = ct, lev = lev, ni = ni)
    
    }
