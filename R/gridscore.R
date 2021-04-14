gridscore <- function(Xtrain, Ytrain, X, Y, score, fun, pars, verb = FALSE) {
  
    ## pars = arguments involved in the calculation of the score
    
    Xtrain <- .mat(Xtrain)
    Ytrain <- .mat(Ytrain, "y")   
    X <- .mat(X)
    Y <- .mat(Y, "y")
    q <- dim(Ytrain)[2]
    
    pars <- data.frame(
        lapply(pars, FUN = function(x) {if(is.factor(x)) as.character(x) else x})
        )
    npar <- dim(pars)[1]
    
    res <- matrix(nrow = npar, ncol = q)
    for(i in seq_len(npar)) {
        zpars <- pars[i, , drop = FALSE]
        if (verb)
            print(zpars)
        fm <- do.call(
            fun, 
            c(list(Xtrain, Ytrain), zpars)
            )
        pred <- predict(fm, X)$pred
        res[i, ] <- score(pred, Y)
        }
    if (verb) 
        cat("/ End. \n\n")
    colnames(res) <- colnames(Ytrain)
    res <- data.frame(pars, res, stringsAsFactors = FALSE)
  
    res
    
    }

gridscorelv <- function(Xtrain, Ytrain, X, Y, score, fun, pars, verb = FALSE) {
  
    ## pars = arguments involved in the calculation of the score
    ## !! Must contains nlv
    
    Xtrain <- .mat(Xtrain)
    Ytrain <- .mat(Ytrain, "y")     
    X <- .mat(X)
    Y <- .mat(Y, "y")
    q <- dim(Ytrain)[2]
    
    pars <- data.frame(
        lapply(pars, FUN = function(x) {if(is.factor(x)) as.character(x) else x})
        )
    nlv <- pars$nlv
    nlv <- seq(min(nlv), max(nlv))
    le_nlv <- length(nlv) 

    ## Only nlv
    if(dim(pars)[2] == 1) {
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
        pars <- pars[ , names(pars) != "nlv", drop = FALSE]
        pars <- unique(pars)
        npar <- dim(pars)[1]
        if(verb) 
            cat("Nb. combinations = ", npar, "\n\n")
        res <- vector(mode = "list", length = npar)
        for(i in seq_len(npar)) {
            zpars <- pars[i, , drop = FALSE]
            if(verb) 
                print(zpars)
            fm <- do.call(
                fun,
                c(list(Xtrain, Ytrain), nlv = max(nlv), zpars)
                )
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
        cat("/ End. \n\n")
  
    res
    
    }

.gridscore_old <- function(Xtrain, Ytrain, X, Y, score, fun, verb = FALSE, ...) {
  
    ## Here the arguments were given independently in ... instead of in pars
    ## ... = arguments involved in the calculation of the score
    dots <- list(...)
    
    Xtrain <- .mat(Xtrain)
    Ytrain <- .mat(Ytrain)     
    q <- dim(Ytrain)[2]
    X <- .mat(X)
    Y <- .mat(Y)
    
    pars <- unique(do.call(
        expand.grid, 
        list(dots, stringsAsFactors = FALSE)
        ))
    npar <- dim(pars)[1]
    
    res <- matrix(nrow = npar, ncol = q)
    for(i in 1:nrow(pars)) {
        zpars <- pars[i, , drop = FALSE]
        if (verb)
            print(zpars)
        fm <- do.call(
            fun, 
            c(list(Xtrain, Ytrain), zpars)
            )
        pred <- predict(fm, X)$pred
        res[i, ] <- score(pred, Y)
        }
    if (verb) 
        cat("/ End. \n\n")
    
    colnames(res) <- paste("y", seq_len(q), sep = "")
    res <- data.frame(pars, res, stringsAsFactors = FALSE)
  
    res
    
    }


