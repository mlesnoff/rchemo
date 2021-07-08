dfplsr_cov <- function(
    X, y, nlv, algo = NULL,
    maxlv = 50, B = 30, 
    print = FALSE, ...) {
    
    if(is.null(algo))
        algo <- plskern
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    nlv <- min(nlv, n, p)
    maxlv <- min(maxlv, n, p)
    
    ## Computation of mu and s2 for the parametric bootstrap
    ## ==> estimated from a low biased model
    ## ----------- s2
    ## Below s2 is not an unbiased estimate of sigma2 for the model
    ## (this unbiased estimate would need to know df, which is actually unknown)
    ## This is not important here, since the amount put in 
    ## the simulated variations is counter-balanced by the covariances.
    ## Efron 2004 p. 620 is not clear how he calculates s2
    ## "obtained from residuals of some 'big' model presumed 
    ## to have negligible bias"
    k <- maxlv
    res <- gridscorelv(X, y, X, y,
                     fun = algo, score = msep, 
                     nlv = seq_len(k), ...)
    ssr <- n * res$y1
    s2 <- ssr[k] / (n - k)
    ## ----------- mu 
    ## In Efron 2004, mu is estimated for each number of LV
    ## This is a simplification here: mu is computed only one time 
    ## from a low-biased model
    #k <- min(20, maxlv)
    fm <- algo(X, y, nlv = k)
    mu <- predict(fm, X)$pred[, 1]
    ## End
    zY <- matrix(rep(mu, B), nrow = n, ncol = B, byrow = FALSE)
    zE <- matrix(rnorm(n * B, sd = sqrt(s2)), nrow = n, ncol = B)
    zY <- zY + zE
    Fit <- array(dim = c(n, B, nlv)) 
    for(j in seq_len(B)) {
        if(print)
            cat(j, " ")
        zfm <- algo(X, zY[, j], nlv = nlv, ...)
        zpred <- predict(zfm, X, nlv = seq_len(nlv))$pred
        for(a in seq_len(nlv))
            Fit[, j, a] <- zpred[[a]][, 1]
        }
    if(print)
        cat("\n\n")
    Cov <- matrix(nrow = n, ncol = nlv)
    for(a in seq_len(nlv))
        for(i in seq_len(n))
            Cov[i, a] <- cov(zY[i, ], Fit[i, , a])
    cov <- colSums(Cov)
    df <- c(1, cov / s2)
    list(df = df, cov = cov)
    }


