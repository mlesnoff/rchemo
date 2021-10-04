odis <- function(
    object, Xtrain, X = NULL, 
    nlv = NULL,
    rob = TRUE, alpha = .01
    ) {
    
    Xtrain <- .mat(Xtrain)
    
    A <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- min(nlv, A)  
    
    E <- xresid(object, Xtrain, nlv = nlv)
    d <- sqrt(rowSums(E * E))
    d2 <- d^2
    if(!rob) {
        mu <- mean(d2)   
        s2 <- var(d2)
        }
    else{
        mu <- median(d2)
        s2 <- mad(d2)^2
        }
    nu <- 2 * mu^2 / s2
    cutoff <- sqrt(mu / nu * qchisq(1 - alpha, df = nu))
    dstand <- d / cutoff 
    res.train <- data.frame(d = d, dstand = dstand)
    rownames(res.train) <- row.names(Xtrain)
    
    res <- NULL
    if(!is.null(X)) {
        X <- .mat(X)
        m <- dim(X)[1]
        E <- xresid(object, X, nlv = nlv)
        d <- sqrt(rowSums(E * E))
        dstand <- d / cutoff 
        res <- data.frame(d = d, dstand = dstand)
        rownames(res) <- row.names(X)
        }
    
    list(res.train = res.train, res = res, cutoff = cutoff)

    }