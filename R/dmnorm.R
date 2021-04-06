dmnorm <- function(X = NULL, mu = NULL, sigma = NULL) {
    
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]

    if(is.null(mu)) 
        mu <- colMeans(X)
    
    if(is.null(sigma)) 
        sigma <- cov(X)
    else
        sigma <- as.matrix(sigma)
    
    ## Cholesky decomposition of sigma
    ## If sigma is singular ==> the option is to replace it by diag(sigma)
    U <- tryCatch(chol(sigma), error = function(e) e)
    if(inherits(U, "error")) 
        U <- sqrt(diag(diag(sigma), nrow = p))
    ## End
    
    ## If U is singular ==> The option is to replace it by I (p, p)
    z <- tryCatch(solve(U), error = function(e) e)
    if(inherits(z, "error")) 
        U <- diag(p)
    ## END
    
    zdet <- det(U)^2
    if(zdet == 0) zdet <- 1e-20
    
    structure(
        list(mu = mu, U = U, det = zdet),
        class = "Dmnorm"
        )
    
    }

predict.Dmnorm <- function(fm, X, ...) {
    
    X <- .mat(X)
    zdim <- dim(X)
    m <- zdim[1]
    p <- zdim[2]

    ## squared distance
    d <- mahsq_mu(X, mu = fm$mu, U = fm$U)
    ## density
    ds <- (2 * pi)^(-p / 2) * (1 / sqrt(fm$det)) * exp(-.5 * d)
    
    dimnames(ds) <- list(row.names(X), "ds")
    
    list(pred = ds)
    
    }

        
    