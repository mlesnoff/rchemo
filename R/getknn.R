getknn <- function(
    Xtrain, X, k = NULL,
    diss = c("eucl", "mahal"), 
    algorithm = "brute", list = TRUE
    ){
    
    diss <- match.arg(diss)
    
    Xtrain <- .mat(Xtrain)
    n <- dim(Xtrain)[1]
    
    X <- .mat(X)
    m <- dim(Xtrain)[1]
    rownam <- row.names(X)

    if(is.null(k)) 
        k <- n
    if(k > n) 
        k <- n
    
    if(diss == "mahal") {
        sigma <- cov(Xtrain)
        U <- chol(sigma)
        Uinv <- solve(U)
        Xtrain <- Xtrain %*% Uinv
        X <- X %*% Uinv
        }

    z <- FNN::get.knnx(Xtrain, X, k = k, algorithm = algorithm)
    nn <- z$nn.index
    d <- z$nn.dist
    row.names(d) <- row.names(nn) <- rownam
    colnames(d) <- colnames(nn) <- seq_len(k)

    listd <- listnn <- NULL
    ##### MAKE lists
    if(list) {
        .fun <- function(x) {
            n <- dim(x)[1]
            z <- vector("list", length = n)
            u <- seq_len(k)
            for(i in seq_len(n)) z[[i]] <- x[i, u]
            names(z) <- row.names(x)
            z
            }
        listnn <- .fun(nn)
        listd <- .fun(d)
        }
    ##### END

    list(nn = nn, d = d, listnn = listnn, listd = listd)
    
    }