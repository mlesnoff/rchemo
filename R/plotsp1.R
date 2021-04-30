plotsp1 <- function(X, col = NULL, zeroes = FALSE, ...) {
    if(is.vector(X))
        X <- matrix(X, nrow = 1)
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(col)) 
        col <- "#045a8d"
    a <- ""
    i <- 1
    while(a == "") {
        a <- readLines(n = 1)
        z <- X[i, , drop = FALSE]
        plotsp(z, col = col, main = i, ...)
        #text(x = min(as.numeric(colnames(z))), 
        #         y = max(z),
        #         pos = 4, labels = i, cex = 1.2)
        if(zeroes)
            abline(h = 0, lty = 2, col = "grey70")
        i <- i + 1
        if(i > nrow(X)) 
            a <- "stop"
    }
}



