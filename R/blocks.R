blockscal <- function(Xtrain, X = NULL, weights = NULL) {
    nblo <- length(Xtrain)
    n <- dim(Xtrain[[1]])[1]
    if(is.null(weights))
        weights <- rep(1 / n, n)
    weights <- .mweights(weights)
    disp <- rep(NA, nblo)
    for(i in seq_len(nblo)) {
        z <- .colvars(Xtrain[[i]], weights = weights)
        disp[i] <- sqrt(sum(z))
        Xtrain[[i]] <- Xtrain[[i]] / disp[i]
        if(!is.null(X))
            X[[i]] <- X[[i]] / disp[i]
        }
    nam <- paste("block", seq_len(nblo), sep = "")
    names(Xtrain) <- nam
    if(!is.null(X))
        names(X) <- nam
    list(Xtrain = Xtrain, X = X, disp = disp)    
    }

mblocks <- function(X, blocks) {
    nblo <- length(blocks)
    zX <- vector(mode = "list", length = nblo)
    for(i in seq(nblo))
        zX[[i]] <- .mat(X[, blocks[[i]], drop = FALSE])
    names(zX) <- paste("block", seq_len(nblo), sep = "")
    zX
    }

hconcat <- function(X)
    do.call(cbind, X)

















