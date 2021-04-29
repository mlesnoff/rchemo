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
    U <- tryCatch(chol(sigma), error = function(e) e)
    if(inherits(U, "error")) {
        ## Temporary - As in rnirs:
        U <- sqrt(diag(diag(sigma), nrow = p))
        ## Alternative: ridge chol
        #lb <- 1e-5
        #U <- chol(sigma + diag(lb, nrow = p, ncol = p))
        }
    ### End
    Uinv <- tryCatch(solve(U), error = function(e) e)
    if(inherits(Uinv, "error")) {
        ## Temporary - As in rnirs:
        Uinv <- solve(diag(diag(sigma), nrow = p))
        ## Alternative: ridge solve
        #lb <- 1e-5
        #Uinv <- solve(U + diag(lb, nrow = p, ncol = p))
        }        
    zdet <- det(U)^2
    if(zdet == 0) 
        zdet <- 1e-20
    structure(
        list(mu = mu, Uinv = Uinv, det = zdet),
        class = "Dmnorm"
        )
    }

predict.Dmnorm <- function(object, X, ...) {
    X <- .mat(X)
    zdim <- dim(X)
    m <- zdim[1]
    p <- zdim[2]
    ## squared distance
    d <- mahsq_mu(X, mu = object$mu, Uinv = object$Uinv)
    ## density
    ds <- (2 * pi)^(-p / 2) * (1 / sqrt(object$det)) * exp(-.5 * d)
    dimnames(ds) <- list(row.names(X), "ds")
    list(pred = ds)
    }

        
    