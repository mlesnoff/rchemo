plotscore <- function(x, y, group = NULL, 
    col = NULL, steplab = 2, legend = TRUE, legend.title = NULL, ncol = 1, ...) {
    dots <- list(...)
    if(is.null(dots$lwd))
        dots$lwd <- 1.8
    fg <- "grey70"
    op <- par()
    right <- left <- 4
    par(mar = c(5, left, 4, right) + 0.1)
    plot(x, y, type = "n", xaxt = "n", las = 1, fg = fg, ...)
    par(mar = op$mar)
    if(!is.null(steplab)) {
        labs <- seq(0, max(x))
        labs[1 + seq(1, max(x), by = steplab)] <- NA
        axis(side = 1, at = labs, labels = labs, fg = fg)
    }
    else
        axis(side = 1)
    if(is.null(group)) {
        if(is.null(col))
            col <- "#045a8d"
        do.call(lines, 
                c(list(x = x, y = y, col = col), dots))
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
     
        for(i in seq_len(nlev)) {
            do.call(lines,
                c(list(x = x[group == levs[i]], y = y[group == levs[i]], 
                       col = col[i]), dots))
        }
        if(legend) {
            if(is.null(legend.title))
                legend.title <- "Group"
            legend("topright", legend = levs,
                box.col = fg, ncol = ncol,
                col = col, lty = 1, xjust = 1, yjust = .5,
                title = legend.title, bty = "n", xpd = TRUE)
        }
        
    }
        
}
