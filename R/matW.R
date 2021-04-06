matW <- function(X, y){
    
    X <- .mat(X)
    n <- dim(X)[1]
    
    lev <-  sort(unique(y))
    nlev <- length(lev)
    ni <- c(table(y))

    ## Case with class(es) with only 1 obs
    if(sum(ni == 1) > 0)
        sigma_1obs <- cov(X) * (n - 1) / n

    zp <- ni / n
    
    Wi <- vector(length = nlev, mode = "list")
    for(i in seq_len(nlev)) {
        
        if(ni[i] == 1)
            Wi[[i]] <- sigma_1obs
        else
            Wi[[i]] <- cov(X[which(y == lev[i]), , drop = FALSE]) * (ni[i] - 1) / ni[i]
        
        colnames(Wi[[i]]) <- rownames(Wi[[i]])
        
        if(i == 1) 
            W <- zp[i] * Wi[[i]] 
        else 
            W <- W + zp[i] * Wi[[i]]
        # Alternative: Could give the weight 0 to the class(es) with 1 obs
        
        }
    names(Wi) <- lev

    list(W = W, Wi = Wi, lev = lev, ni = ni)
    
    }

matB <- function(X, y) {
    
    z <- aggmean(X, y)
    B <- .colcovs(z$ct, weights = z$ni)
    colnames(B) <- rownames(B) <- colnames(z$ct)

    list(B = B, ct = z$ct, lev = z$lev, ni = z$ni)
    
    }



