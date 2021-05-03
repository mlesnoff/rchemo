plotxy <- function(
    X, group = NULL, 
    asp = 0, col = NULL, alpha.f = .8,
    zeroes = FALSE, circle = FALSE, ellipse = FALSE,
    labels = FALSE,
    legend = TRUE, legend.title = NULL, ncol = 1, ...
    ) {
    X <- as.data.frame(X[, seq_len(2)])
    rownam <- row.names(X)
    fg <- "grey70"
    op <- par()
    left <- 4 ; right <- 4
    par(mar = c(5, left, 4, right) + 0.1)
    plot(X, 
        type = "n", xaxt = "n",
        las = 1, fg = fg,
        asp = asp, ...)
    axis(side = 1, fg = fg, asp = asp, ...)
    par(mar = op$mar)
    if(zeroes)
        abline(h = 0, v = 0, lty = 2, col = fg)
    if(circle)
        lines(.ellips(diag(2), c(0, 0), 1)$X, col = fg)
    if(is.null(group)) {
        if(is.null(col))
            col <- "#045a8d"
        col <- adjustcolor(col, alpha.f)
        if(!labels)
            points(X, col = col, ...)
        else
            text(X[, 1], X[, 2], rownam, col = col, ...)
        if(ellipse)
            lines(.ellips(cov(X), .colmeans(X), sqrt(qchisq(.95, df = 2)))$X, col = "grey")
    }
    else {
        if(!is.factor(group))
            group <- as.factor(as.character(group))
        levs <- levels(group)
        nlev <- length(levs)
        if(!is.null(col)){
            if(length(col) == 1)
                col <- rep(col, nlev)
        }
        else
            col <- palette.colors(n = nlev, palette = "ggplot2", recycle = TRUE)
        col <- adjustcolor(col, alpha.f)
        for(i in seq_len(nlev)) {
            z <- X[group == levs[i], , drop = FALSE]
            zrownam <- row.names(z)
            if(!labels)
                points(z, col = col[i], ...)
            else
                text(z[, 1], z[, 2], zrownam, col = col[i], ...)
            if(ellipse)
                lines(.ellips(cov(z), .colmeans(z), sqrt(qchisq(.95, df = 2)))$X, col = col[i])
        }
        if(legend) {
            if(is.null(legend.title))
                legend.title <- "Group"
            pch <- list(...)$pch
            if(is.null(pch))
                pch <- 1
            legend("topright", legend = levs,
                box.col = fg, ncol = ncol,
                col = col, pch = pch, xjust = 1, yjust = 0.5,
                title = legend.title, bty = "n", xpd = TRUE)
        }
    }
}
