sampdp <- function(X, k, diss = c("eucl", "mahal")) {

    diss <- match.arg(diss)
    
    X <- .mat(X)
    n <- dim(X)[1]
    zn <- seq_len(n)
    
    if(k > round(n / 2))
        stop("\n\n Argument 'k' must be <= half of the data. \n\n") 
    
    if(diss == "eucl")
        D <- euclsq(X)
    
    if(diss == "mahal")
        D <- mahsq(X)
    
    colnames(D) <- rownames(D) <- zn
    
    ## initial 2 selections 
    ## train
    s1 <- which(D == max(D), arr.ind = TRUE)[1, ]
    zD <- D
    zD[s1, ] <- zD[, s1] <- NA
    ## test
    s2 <- which(D == max(zD, na.rm = TRUE), arr.ind = TRUE)[1, ]
    
    ## candidates
    cand <- seq_len(n)[-c(s1, s2)]
    
    for(i in seq_len(k - 2)) {
        zD <- D[s1, cand, drop = TRUE]
        u <- apply(zD, MARGIN = 2, FUN = min)
        zs <- cand[which(u == max(u))[1]]
        s1 <- c(s1, zs)
        cand <- zn[-c(s1, s2)]
        zD <- D[s2, cand, drop = TRUE]
        u <- apply(zD, MARGIN = 2, FUN = min)
        zs <- cand[which(u == max(u))[1]]
        s2 <- c(s2, zs)
        cand <- zn[-c(s1, s2)]
        }
    
    names(s2) <- names(s1) <- NULL
    
    
    list(train = s1, test = s2, remain = cand)

    }
