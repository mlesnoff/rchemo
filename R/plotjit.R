plotjit <- function(
    x, y, group = NULL, 
    jit = 1, col = NULL, alpha.f = .8,
    legend = TRUE, legend.title = NULL, ncol = 2, med = TRUE,
    ...
    ) {
    
    if(!is.factor(x))
        x <- as.factor(x)
    
    zx <- as.numeric(x)
    Class <- jitter(zx, factor = jit)
    ncla <- length(levels(x))
    
    fg <- "grey70"
    plot(Class, y,
        type = "n", xaxt = "n",
        las = 1, fg = fg,
        ...
        )
    axis(side = 1, fg = fg, at = seq_len(ncla), 
             labels = as.character(levels(x)))
    
    if(is.null(group)) {
        if(is.null(col))
            col <- "#045a8d"
        col <- adjustcolor(col, alpha.f)
        points(Class, y, col = col, ...)
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
            zClass <- Class[group == levs[i]]
            zy <- y[group == levs[i]]
            points(zClass, zy, col = col[i], ...)
            }
        if(legend) {
            if(is.null(legend.title))
                legend.title <- "Group"
            pch <- list(...)$pch
            if(is.null(pch))
                pch <- 1
            legend("topright", legend = levs,
                box.col = fg, ncol = ncol,
                col = col, pch = pch, xjust = 1, yjust = 1,
                title = legend.title)
            }
        }
    
    if(med) {
        zx <- unique(x)
        for(i in seq_len(length(zx))) {
            s <- x == zx[i]
            points(mean(Class[s]), median(y[s]), col = "blue", pch = 16, cex = 1.5)
            }
        }

    }
