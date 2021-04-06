sampks <- function(X, k, diss = c("eucl", "mahal")) {

    diss <- match.arg(diss)
    
    X <- .mat(X)
    n <- dim(X)[1]
    zn <- seq_len(n)
    
    if(diss == "eucl")
        D <- euclsq(X)
    
    if(diss == "mahal")
        D <- mahsq(X)
    
    colnames(D) <- rownames(D) <- zn
    
    ## initial 2 selections (train)
    s <- which(D == max(D), arr.ind = TRUE)[1, ]
    ## candidates
    cand <- zn[-s]
    
    ## The following part is not time-efficient for k > 200
    for(i in seq_len(k - 2)) {
        u <- apply(D[s, cand, drop = TRUE], MARGIN = 2, FUN = min)    
        zs <- cand[which(u == max(u))[1]]
        s <- c(s, zs)
        cand <- zn[-s]
        }
    ## End
    
    names(s) <- NULL
    
    list(train = s, test = cand)

    }
