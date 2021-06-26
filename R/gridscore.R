gridscore <- function(Xtrain, Ytrain, X, Y, score, fun, pars, verb = FALSE) {
    ## pars = List of named vectors (arguments) involved in the calculation of the score
    Xtrain <- .mat(Xtrain)
    Ytrain <- .mat(Ytrain, "y")   
    X <- .mat(X)
    Y <- .mat(Y, "y")
    q <- dim(Ytrain)[2]
    nco <- length(pars[[1]])
    npar <- length(pars)
    if(verb) 
        cat("-- Nb. combinations = ", nco, "\n")
    res <- matrix(nrow = nco, ncol = q)
    for(i in seq_len(nco)) {
        zpars <- lapply(pars, FUN = function(x) x[[i]])
        if (verb)
            print(data.frame(zpars))
        fm <- do.call(
            fun, 
            c(list(Xtrain, Ytrain), zpars))
        pred <- predict(fm, X)$pred
        res[i, ] <- score(pred, Y)
    }
    if (verb) 
        cat("-- End. \n\n")
    colnames(res) <- colnames(Ytrain)
    res <- data.frame(pars, res, stringsAsFactors = FALSE)
    res
}

gridscorelv <- function(Xtrain, Ytrain, X, Y, score, fun, nlv, pars = NULL, verb = FALSE) {
    ## pars = List of named vectors (arguments) involved in the calculation of the score
    ## Must not contains nlv
    Xtrain <- .mat(Xtrain)
    Ytrain <- .mat(Ytrain, "y")     
    X <- .mat(X)
    Y <- .mat(Y, "y")
    q <- dim(Ytrain)[2]
    nlv <- seq(min(nlv), max(nlv))
    le_nlv <- length(nlv) 
    ## Case where pars is NULL
    if(is.null(pars)) {
        if(verb) 
            cat("-- Nb. combinations = 0 \n")
        fm <- fun(Xtrain, Ytrain, nlv = max(nlv))
        pred <- predict(fm, X, nlv = nlv)$pred
        if(le_nlv == 1)
            pred <- list(pred)
        res <- matrix(nrow = le_nlv, ncol = q)
        for(i in seq_len(le_nlv))
            res[i, ] <- score(pred[[i]], Y)
        colnames(res) <- colnames(Ytrain)
        res <- data.frame(nlv = nlv, res, stringsAsFactors = FALSE)
    } 
    ## End
    else {
        npar <- length(pars)
        nco <- length(pars[[1]])
        if(verb) 
            cat("-- Nb. combinations = ", nco, "\n")
        res <- vector(mode = "list", length = nco)
        for(i in seq_len(nco)) {
            zpars <- lapply(pars, FUN = function(x) x[[i]])
            if (verb)
                print(data.frame(zpars))
            fm <- do.call(
                fun,
                c(list(Xtrain, Ytrain), nlv = max(nlv), zpars))
            zpred <- predict(fm, X, nlv = nlv)$pred
            if(le_nlv == 1)
                zpred <- list(zpred)
            zres <- matrix(nrow = le_nlv, ncol = q)
            for(j in seq_len(le_nlv))
                zres[j, ] <- score(zpred[[j]], Y)
            colnames(zres) <- colnames(Ytrain)
            zres <- data.frame(nlv = nlv, zres, stringsAsFactors = FALSE)
            res[[i]] <- suppressWarnings(data.frame(zpars, zres)) 
        }
        
        res <- setDF(rbindlist(res))    
    }
    if (verb) 
        cat("-- End. \n\n")
    res
}

gridscorelb <- function(Xtrain, Ytrain, X, Y, score, fun, lb, pars = NULL, verb = FALSE) {
    ## pars = List of named vectors (arguments) involved in the calculation of the score
    ## Must not contains lb
    Xtrain <- .mat(Xtrain)
    Ytrain <- .mat(Ytrain, "y")     
    X <- .mat(X)
    Y <- .mat(Y, "y")
    q <- dim(Ytrain)[2]
    lb <- sort(unique(lb))
    le_lb <- length(lb) 
    ## Case where pars is empty
    if(is.null(pars)) {
        if(verb) 
            cat("-- Nb. combinations = 0 \n")
        fm <- fun(Xtrain, Ytrain, lb = max(lb))
        pred <- predict(fm, X, lb = lb)$pred
        if(le_lb == 1)
            pred <- list(pred)
        res <- matrix(nrow = le_lb, ncol = q)
        for(i in seq_len(le_lb))
            res[i, ] <- score(pred[[i]], Y)
        colnames(res) <- colnames(Ytrain)
        res <- data.frame(lb = lb, res, stringsAsFactors = FALSE)
    } 
    ## End
    else {
        npar <- length(pars)
        nco <- length(pars[[1]])
        if(verb) 
            cat("-- Nb. combinations = ", nco, "\n")
        res <- vector(mode = "list", length = nco)
        for(i in seq_len(nco)) {
            zpars <- lapply(pars, FUN = function(x) x[[i]])
            if(verb) 
                print(data.frame(zpars))
            fm <- do.call(
                fun,
                c(list(Xtrain, Ytrain), lb = max(lb), zpars)
                )
            zpred <- predict(fm, X, lb = lb)$pred
            if(le_lb == 1)
                zpred <- list(zpred)
            zres <- matrix(nrow = le_lb, ncol = q)
            for(j in seq_len(le_lb))
                zres[j, ] <- score(zpred[[j]], Y)
            colnames(zres) <- colnames(Ytrain)
            zres <- data.frame(lb = lb, zres, stringsAsFactors = FALSE)
            res[[i]] <- suppressWarnings(data.frame(zpars, zres)) 
        }
        res <- setDF(rbindlist(res))    
    }
    if (verb) 
        cat("-- End. \n\n")
    res
}

