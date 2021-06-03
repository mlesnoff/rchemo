locw <- function(Xtrain, Ytrain, X, listnn, listw = NULL, fun, verb = FALSE, ...) {
    X <- .mat(X)
    m <- dim(X)[1]
    Ytrain <- .mat(Ytrain)
    q <- dim(Ytrain)[2]
    pred <- matrix(nrow = m, ncol = q)
    for(i in seq_len(m)) {
        if(verb)
            cat(i, " ")
        s <- listnn[[i]]
        zYtrain <- Ytrain[s, , drop = FALSE]
        ##----- For discrimination, 
        ## case where all the neighbors are of same class
        nval <- length(unique(zYtrain))
        if(q == 1 & nval == 1) {
            fm <- NULL
            pred[i, ] <- zYtrain[1]
            }
        ##----- End
        else {
            if(is.null(listw))
                fm <- fun(Xtrain[s, , drop = FALSE], zYtrain, ...)
            else
                fm <- fun(Xtrain[s, , drop = FALSE], zYtrain, 
                          weights = listw[[i]], ...)
            pred[i, ] <- predict(fm, X[i, , drop = FALSE])$pred
            }
        }
    if(verb)
        cat("\n")
    rownam <- row.names(X)
    colnam <- paste("y", seq_len(q), sep = "")
    dimnames(pred) <- list(rownam, colnam)
    list(pred = pred)
    }

locwlv <- function(Xtrain, Ytrain, X, listnn, listw = NULL, fun, nlv, verb = FALSE, ...) {
    X <- .mat(X)
    m <- dim(X)[1]
    Ytrain <- .mat(Ytrain)
    q <- dim(Ytrain)[2]
    nlv <- seq(min(nlv), max(nlv))
    le_nlv <- length(nlv)
    res <- array(dim = c(m, q, le_nlv))
    for(i in seq_len(m)) {
        if(verb)
            cat(i, " ")
        s <- listnn[[i]]
        zYtrain <- Ytrain[s, , drop = FALSE]
        nval <- length(unique(zYtrain))
        ##----- For discrimination, 
        ## case where all the neighbors are of same class
        if(q == 1 & nval == 1) {
            fm <- NULL
            for(a in seq_len(le_nlv)) 
                res[i, , a] <- zYtrain[1]
            }
        ##----- End
        else {   
            if(is.null(listw))
                fm <- fun(Xtrain[s, , drop = FALSE], zYtrain, 
                          nlv = max(nlv), ...)
            else
                fm <- fun(Xtrain[s, , drop = FALSE], zYtrain, 
                          weights = listw[[i]], nlv = max(nlv), ...)
            for(a in seq_len(le_nlv)) 
                res[i, , a] <- predict(fm, X[i, , drop = FALSE], nlv = nlv[a])$pred
            }
        }
    if(verb)
        cat("\n")
    rownam <- row.names(X)
    colnam <- paste("y", seq_len(q), sep = "")
    pred <- vector("list", length = le_nlv)
    for(a in seq_len(le_nlv)) {
        z <- res[, , a, drop = FALSE]
        zdim <- dim(z)
        z <- matrix(z, nrow = zdim[1], ncol = zdim[2], 
            dimnames = list(rownam, colnam))
        pred[[a]] <- z
        }
    names(pred) <- paste("lv", nlv, sep = "")
    if(le_nlv == 1)
        pred <- pred[[1]]  
    list(pred = pred)
    }
   





    