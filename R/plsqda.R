plsqda <- function(X, y, nlv, weights = NULL, prior = c("unif", "prop")) {
    
    if(is.factor(y))
        y <- as.character(y)
    
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    nlv <- min(nlv, n, p)
    
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    
    Y <- dummy(y)$Y
    
    fm <- list()
    fm[[1]] <- plskern(X, Y, nlv = nlv, weights = weights)
    ## Should be:
    ## z <- transform(fm[[1]], X)
    ## But same as:
    z <- fm[[1]]$T
    fm[[2]] <- vector(length = nlv, mode = "list")
    for(i in seq_len(nlv))
      fm[[2]][[i]] <- qda(z[, seq_len(i), drop = FALSE], y, prior = prior)
    
    structure(fm, class = "Plsdaprob")       

    }
