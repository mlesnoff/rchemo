scordis <- function(
    object, X = NULL, 
    nlv = NULL,
    rob = TRUE, alpha = .01
    ) {
    
    A <- dim(object$T)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- min(nlv, A)  
    
    n <- dim(object$T)[1]
    S <- cov(object$T[, seq(nlv), drop = FALSE]) * (n - 1) / n
    Uinv <- solve(chol(S))
    d2 <- c(mahsq_mu(object$T[, seq(nlv), drop = FALSE], 
                   mu = rep(0, nlv), Uinv = Uinv))
    d <- sqrt(d2)
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
    res.train <- data.frame(d = d, dstand = dstand, gh = d2 / nlv)
    rownames(res.train) <- row.names(object$T)
    
    res <- NULL
    if(!is.null(X)) {
        T <- transform(object, X, nlv = nlv)
        d2 <- c(mahsq_mu(T, mu = rep(0, nlv), Uinv = Uinv))
        d <- sqrt(d2)
        dstand <- d / cutoff 
        res <- data.frame(d = d, dstand = dstand, gh = d2 / nlv)
        rownames(res) <- row.names(X)
        }

    list(res.train = res.train, res = res, cutoff = cutoff)
    
    }


