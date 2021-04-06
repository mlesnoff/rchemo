sampclas <- function(x, y = NULL, n) {
    
    zn <- length(x)

    lev <-  sort(unique(x))
    nlev <- length(lev)
    ni <- c(table(x))

    if(length(n) == 1) 
        n <- rep(n, nlev)
    else
        if(length(n) != nlev)
            stop("\n\n    Length of argument 'n' must be =1 
                 or =the number of classes in vector'x'. \n\n") 
    
    s <- list()
    for(i in seq_len(nlev)) {
        n[i] <- min(n[i], ni[i])
        zs <- which(x == lev[i])
        if(is.null(y)) {
            s[[i]] <- sample(zs, size = n[i], replace = FALSE)
            } 
        else {
            zy <- y[zs]
            ## "order(y) = 3 7 etc." means that the 1st lower value is the component 3 of the vector,
            ## the 2nd lower value is the component 7 of the vector,
            id <- order(zy)
            u <- round(seq(1, ni[i], length = n[i]))
            s[[i]] <- zs[id[u]]
            }
        }
    s <- unlist(s)
    
    list(train = s, test = seq_len(zn)[-s], lev = lev, ni = ni)
    
    }
