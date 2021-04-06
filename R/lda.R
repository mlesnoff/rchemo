lda <- function(X, y, prior = c("unif", "prop")) {
    
    prior <- match.arg(prior)
    if(is.factor(y))
        y <- as.character(y)
    
    X <- .mat(X)
    n <- dim(X)[1]
    
    res <- aggmean(X, y)
    ct <- res$ct
    lev <- res$lev
    nlev <- length(lev)
    ni <- res$ni
    
    if(prior == "unif")
        wprior <- rep(1 / nlev, nlev)
    
    if(prior == "prop")
        wprior <- ni / sum(ni)
    
    res <- matW(X, y)
    W <- res$W * n / (n - nlev)
    
    structure(
        list(ct = ct, W = W, wprior = wprior, 
             lev = lev, ni = ni),
        class = c("Lda")
        )       

    }

predict.Lda <- function(fm, X, ...) {
    
    X <- .mat(X)
    m <- dim(X)[1]

    lev <- fm$lev
    nlev <- length(lev) 
    
    ds <- matrix(nrow = m, ncol = nlev)
    for(i in seq_len(nlev)) {
        zfm <- dmnorm(X, mu = fm$ct[i, ], sigma = fm$W) 
        ds[, i] <- predict(zfm, X)$pred
        }    
    
    z <- t(fm$wprior * t(ds))
    posterior <- z / rowSums(z)
    
    z <- apply(posterior, FUN = .findmax, MARGIN = 1)
    pred <- matrix(.replace_bylev(z, lev), ncol = 1)
    
    rownam <- row.names(X)
    dimnames(pred) <- list(rownam, "y1")
    dimnames(posterior) <- dimnames(ds) <- list(rownam, lev)
    
    list(pred = pred, ds = ds, posterior = posterior)
    
    }    
    