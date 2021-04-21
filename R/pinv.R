pinv <- function(X, tol = sqrt(.Machine$double.eps)) {
    X <- .mat(X)
    fm <- svd(X)
    posit <- fm$d > max(tol * fm$d[1L], 0)
    fm$d[!posit] <- 0
    sv <- fm$d[posit]
    V <- fm$v[, posit, drop = FALSE]
    U <- fm$u[, posit, drop = FALSE]
    Xplus <- V %*% (1 / sv * t(U))
    list(Xplus = Xplus, sv = fm$d)
    }







