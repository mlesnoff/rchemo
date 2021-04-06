dummy <- function(y) {
    
    ## Works also if y is a factor
    
    if(sum(is.na(y)) > 0)    stop("NA in 'y' are not allowed")
    
    lev <-  sort(unique(y))
    nlev <- length(lev)
    ni <- c(table(y))

    n <- length(y)
    
    z <- matrix(nrow = n, ncol = nlev) 
    for(i in seq_len(nlev))
        z[, i] <- as.numeric(y == lev[i])
        
    dimnames(z) <- list(seq_len(n), lev)

    list(Y = z, lev = lev, ni = ni)
    
    }

