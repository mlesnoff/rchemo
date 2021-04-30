plotsp <- function(
    X,
    type = "l", col = NULL, zeroes = FALSE, labels = FALSE, 
    add = FALSE,
    ...
    ) {
    dots <- list(...)
    if(is.vector(X))
        X <- matrix(X, nrow = 1)
    X <- .mat(X)
    zdim <- dim(X)
    n <- zdim[1]
    p <- zdim[2]
    colnam <- suppressWarnings(as.numeric(colnames(X)))
    if(sum(is.na(colnam)) > 0) 
        colnam <- seq_len(p)
    if(is.null(col)) 
        col <- rep("#045a8d", n)
    else
        if(length(col) == 1)
            col <- rep(col, n)
    if(is.null(dots$xlim)) {
        u <- 0.03
        dots$xlim <- c((1 - u) * min(colnam), (1 + u) * max(colnam))
    } 
    if(is.null(dots$ylim)) {
        u <- 0.05
        dots$ylim <- c((1 - u) * min(X), (1 + u) * max(X))
    } 
    if(is.null(dots$xlab)) 
        dots$xlab <- "x-value" 
    if(is.null(dots$ylab)) 
        dots$ylab <- "y-value" 
    .flines <- function(X, colnam, type, col, labels = FALSE, ...) {
        m <- dim(X)[1]
        for(i in seq_len(m)) {
            lines(x = colnam, y = X[i, ], type = type, col = col[i], ...)
            if(labels) {
                z <- p # round(.99 * p)
                text(x = 1.02 * colnam[z], y = X[i, z],
                    labels = row.names(X)[i], col = col[i], cex = .8)
            }
        }
    }
    if(!add) {
        fg <- "grey70"
        do.call(plot,
            c(list(x = colnam, y = X[1, ], type = "n", las = 1, fg = fg), dots))
        if(zeroes)
            abline(h = 0, lty = 2, col = fg)
    }
    .flines(X, colnam, type, col, labels, ...) 
    }

