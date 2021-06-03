eposvd <- function(D, nlv) {
    D <- .mat(D)
    zdim <- dim(D)
    m <- zdim[1]
    p <- zdim[2]
    nlv <- min(nlv, m, p)
    I <- diag(1, nrow = p, ncol = p)
    if(nlv == 0) { 
        M <- I
        P <- NULL
        }
    else {
        P <- svd(D)$v[, 1:nlv, drop = FALSE]
        M <- I - tcrossprod(P)
    }
    list(M = M, P = P)
}
