pcasvd <- function(X, nlv, weights = NULL) {
  
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
  
    nlv <- min(nlv, n, p)
  
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
  
    xmeans <- .colmeans(X, weights = weights)
    X <- .center(X, xmeans)
  
    res <- svd(sqrt(weights) * X, nu = 0, nv = nlv)
    P <- res$v
    sv <- res$d[seq_len(min(n, p))]
    sv[sv < 0] <- 0
  
    T <- X %*% P
    ## If weights > 0
    ## = 1 / sqrt(weights) * res$u %*% diag(sv, nrow = nlv)
  
    eig <- sv^2         
    ## eig
    ## = eigenvalues of X'DX = Cov(X) in metric D 
    ## = variances of scores T in metric D
    ## = TT = colSums(weights * T * T)  
    ## = norms^2 of the scores T in metric D
    ## = .xnorm(T, weights = weights)^2   
  
    row.names(T) <- row.names(X)
    row.names(P) <- colnames(X)
  
    colnames(T) <- colnames(P) <- paste("pc", seq_len(nlv), sep = "")
  
    structure(
        list(T = T, P = P, sv = sv, eig = eig, 
            xmeans = xmeans, weights = weights, niter = NULL, conv = NULL),
        class = c("PcaOrtho", "Pca")
        )

    }

transform <- function(object, X, ...) UseMethod("transform")

transform.Pca <- function(object, X, ..., nlv = NULL) {
    A <- dim(object$P)[2]
    if(is.null(nlv))
        nlv <- A
    else 
        nlv <- min(nlv, A)
    T <- .center(.mat(X), 
                 object$xmeans) %*% object$P[, seq_len(nlv), drop = FALSE]
    colnames(T) <- paste("pc", seq_len(dim(T)[2]), sep = "")
    T
    }




