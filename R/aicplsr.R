aicplsr <- function(
    X, y, nlv, algo = NULL,
    meth = c("cg", "div", "cov"),
    correct = TRUE, B = 50, 
    print = FALSE, ...) {
    meth <- match.arg(meth) 
    if(is.null(algo))
        algo <- plskern
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    nlv <- min(nlv, n, p)
    res <- gridscorelv(X, y, X, y,
                       fun = algo, score = msep, 
                       nlv = seq(0, nlv), ...)
    ssr <- n * res$y1
    if(meth == "cg") 
        df <- dfplsr_cg(
            X, y, nlv = nlv, reorth = TRUE)$df 
    if(meth == "div") 
        df <- dfplsr_div(
            X, y, nlv = nlv, algo = algo, B = B, 
            print = print, ...)$df    
    if(meth == "cov") 
        df <- dfplsr_cov(
            X, y, nlv = nlv, algo = algo, B = B, 
            print = print, ...)$df
    df.ssr <- n - df
    ## For Cp, unbiased estimate of sigma2 
    ## ----- Cp1: From a low biased model
    ## Not stable with dfcov and nlv too large compared to best model !!
    ## If df stays below .95 * n, this corresponds
    ## to the maximal model (nlv)
    ## Option 2 gives in general results
    ## very close to those of option 1,
    ## but can give poor results with dfcov
    ## when nlv is set too large to the best model
    k <- max(which(df <= .50 * n))
    s2.1 <- ssr[k] / df.ssr[k]
    ## ----- Cp2: FPE-like
    ## s2 is estimated from the model under evaluation
    ## Used in Kraemer & Sugiyama 2011 Eq.5-6
    s2.2 <- ssr / df.ssr
    ## Option 3: From a low biased model (alternative)
    ## The following option can generate poor Cp estimates 
    ## if nlv is over-large for small data
    ## ==> not used
    #if(option == 3) {
    #    minlv <- 15
    #    k <- min(minlv, nlv)
    #    u <- (k + 1):(nlv + 1)
    #    s2 <- median(ssr[u] / (n - df[u]))
    #    }
    ## End
    ct <- rep(1, nlv + 1)
    if(correct) 
        ct <- n / (n - df - 2)
    ct[df > n | ct <= 0] <- NA 
    ## For safe predictions when df stabilizes 
    ## and fluctuates
    ct[df > .80 * n] <- NA
    ## End
    u <- which(is.na(ct))
    if(length(u) > 0)
        ct[seq(min(u), nlv + 1)] <- NA
    aic <- n * log(ssr) + 2 * (df + 1) * ct
    cp1 <- ssr + 2 * s2.1 * df * ct
    cp2 <- ssr + 2 * s2.2 * df * ct
    #fpe <- ssr * (n + df) / (n - df) * ct
    cp1 <- cp1 / n
    cp2 <- cp2 / n
    crit <- data.frame(aic = aic, cp1 = cp1, cp2 = cp2)
    delta <- data.frame(nlv = seq(0, nlv),
        apply(crit, MARGIN = 2, FUN = function(x) x - min(x, na.rm = TRUE)))
    opt <- apply(
        crit, MARGIN = 2,
        FUN = function(x) which(x == min(x, na.rm = TRUE))[1] - 1)
    crit <- data.frame(
        nlv = seq(0, nlv), n = rep(n, nlv + 1),
        df = df, ct = ct,
        ssr = ssr, 
        crit)
    list(crit = crit, delta = delta, opt = opt)
    }
