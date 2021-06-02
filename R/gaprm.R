gaprm <- function(X, numcol, k = 5) {
    if(is.vector(X))
        X <- matrix(X, nrow = 1)
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    colnam <- suppressWarnings(as.numeric(colnames(X)))
    if(sum(is.na(colnam)) > 0) 
        colnam <- seq_len(p)
    for (i in 1:length(numcol)) {
        zcol <- numcol[i]
        w <- seq(zcol - k + 1, zcol)
        fm <- lm(t(X[, w, drop = FALSE]) ~ w)
        B <- coef(fm)
        pred <- t(c(1, zcol + 1)) %*% B
        bias <- X[, zcol + 1] - c(pred)
        X[, (zcol + 1):p] <- X[, (zcol + 1):p] - bias
    }
    X
}
