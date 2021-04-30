plotxna <- function(X, pch = 16, col = "red", grid = FALSE, asp = 0, ...) {
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    z <- which(is.na(X), arr.ind = TRUE) 
    fg <- "grey70"
    eps <- .1
    xlim <- c(1 - eps, p + eps)
    ylim <- c(1 - eps, n + eps)
    if(n < 50)
        xlength <- p
    else
        xlength <- 20
    if(p < 50)
        ylength <- p
    else
        ylength <- 20
    xlabs <- round(seq(1, p, length = xlength))
    ylabs <- round(seq(1, n, length = ylength))
    plot(
        NULL,
        asp = asp,
        fg = fg,
        xaxt = "n", yaxt = "n", xaxs = "i",
        xlim = xlim, ylim =    ylim,
        xlab = "Columns", ylab = "Rows",
        ...)
    axis(side = 1, at = xlabs, labels = xlabs)
    axis(side = 2, at = ylabs, labels = ylabs, las = 1)
    points(z[, 2], z[, 1], pch = pch, col = col)
    if(grid)
        abline(h = seq_len(n), v = seq_len(p), col = "grey")
    }

