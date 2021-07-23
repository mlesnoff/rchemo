rmgap <- function(X, indexcol, k = 5) {
    if(is.vector(X))
        X <- matrix(X, nrow = 1)
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    colnam <- suppressWarnings(as.numeric(colnames(X)))
    if(sum(is.na(colnam)) > 0) 
        colnam <- seq_len(p)
    for (i in 1:length(indexcol)) {
        ind <- indexcol[i]
        w <- seq(max(ind - k + 1, 1), ind)
        fm <- lm(t(X[, w, drop = FALSE]) ~ w)
        B <- coef(fm)
        pred <- t(c(1, ind + 1)) %*% B
        bias <- X[, ind + 1] - c(pred)
        X[, (ind + 1):p] <- X[, (ind + 1):p] - bias
    }
    X
}
