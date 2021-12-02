sampcla <- function(x, y = NULL, m) {
    
    n <- length(x)

    lev <-  sort(unique(x))
    nlev <- length(lev)
    ni <- c(table(x))

    if(length(m) == 1) 
        m <- rep(m, nlev)
    else
        if(length(m) != nlev)
            stop("\n\n    Length of argument 'm' must be =1 
                 or =the number of classes in vector'x'. \n\n") 
    
    s <- list()
    for(i in seq_len(nlev)) {
        m[i] <- min(m[i], ni[i])
        zs <- which(x == lev[i])
        if(is.null(y)) {
            s[[i]] <- sample(zs, size = m[i], replace = FALSE)
            } 
        else {
            zy <- y[zs]
            ## "order(y) = 3 7 etc." means that the 1st lower value is the component 3 of the vector,
            ## the 2nd lower value is the component 7 of the vector,
            id <- order(zy)
            u <- round(seq(1, ni[i], length = m[i]))
            s[[i]] <- zs[id[u]]
            }
        }
    s <- unlist(s)
    
    list(train = seq_len(n)[-s], test = s, lev = lev, ni = ni)
    
    }
