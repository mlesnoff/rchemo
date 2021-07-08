dfplsr_div <- function(
    X, y, nlv, algo = NULL, 
    eps = 1e-2, B = 30, 
    print = FALSE, ...) {
    if(is.null(algo))
        algo <- plskern
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    y <- c(y)
    nlv <- min(nlv, n, p)
    B <- min(B, n) # nb. obs with divergence
    s <- sample(seq_len(n), size = B, replace = FALSE)
    eps <- mean(y) * eps
    fm <- algo(X, y, nlv = nlv, ...)
    pred <- predict(fm, X, nlv = seq_len(nlv))$pred
    S <- matrix(nrow = B, ncol = nlv)
    for(i in seq_len(B)) {
        if(print)
            cat(i, " ")
        zs <- s[i]
        zy <- y
        zy[zs] <- y[zs] + eps
        zfm <- algo(X, zy, nlv = nlv, ...)
        zpred <- predict(zfm, X[zs, , drop = FALSE], nlv = seq_len(nlv))$pred
        v <- numeric()
        for(a in seq_len(nlv))
            v[a] <- zpred[[a]] - pred[[a]][zs, ]
        S[i, ] <- v / eps
        }
    if(print)
        cat("\n\n")
    df <- colSums(S) * n / B
    df <- c(1, df)
    list(df = df)
    }


