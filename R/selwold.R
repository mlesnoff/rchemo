selwold <- function(
    r, indx = seq(length(r)), 
    smooth = TRUE, f = 1/3,
    alpha = .05, digits = 3,
    plot = TRUE,
    xlab = "Index", ylab = "Value", main = "r",
    ...
    ) {
    n <- length(r)
    zdiff <- -diff(r)
    R <- zdiff / abs(r[-length(r)])
    zdiff <- c(zdiff, NA)
    Rs <- R <- c(R, NA)
    if(smooth)
        Rs <- lowess(indx, R, f = f, ...)$y
    opt <- indx[r == min(r)][1]
    sel <- indx[Rs < alpha][1]
    if(is.na(sel))
        sel <- opt
    sel <- min(opt, sel)
    res <- data.frame(indx = indx, r = r, diff = -zdiff)
    res$R <- round(R, digits = digits)
    res$Rs <- round(Rs, digits = digits)
    row.names(res) <- seq_len(n)
    if(plot) {
        fg <- "grey70" ; col <- "#045a8d"
        xmin <- min(indx) ; xmax <- max(indx)
        eps <- .8
        if(n <= 55)
            labs <- seq(xmin, xmax, by = 2)
        else
            labs <- seq(xmin, xmax, by = 10)
        op <- par()
        right <- left <- 4
        par(mfrow = c(1, 2), mar = c(5, left, 4, right) + 0.1)
        plot(
            indx, r, 
            type = "l", col = col, pch = 16,
            xaxt = "n", las = 1, fg = fg, las = 1, 
            xlim = c(xmin - eps, xmax + eps), xaxs = "i",
            xlab = xlab, ylab = ylab, main = main)
        points(indx, r, pch = 16, col = col)
        if(sel < opt)
            points(seq(sel, opt), r[indx %in% seq(sel, opt)], 
                   pch = 16, col = "grey", cex = 1.2)
        points(opt, r[indx == opt], pch = 16, col = "red", cex = 1.2)
        axis(side = 1, at = labs, labels = labs, fg = fg)
        abline(h = min(r), col = "grey")
        plot(
            indx, R, 
            type = "l", pch = 16, col = col,
            xaxt = "n", las = 1, fg = fg, las = 1,
            xlim = c(xmin - eps, xmax - 1 + eps), xaxs = "i",
            xlab = xlab, ylab = "R", main = "Relative gain")
        points(indx, R, pch = 16, col = col)
        if(smooth) {
            lines(indx, Rs, type = "l", col = "red")
            legend("topright", legend = c("Raw", "Smoothed"),
                box.col = "grey70", ncol = 1,
                col = c("#045a8d", "red"), lty = 1, 
                xjust = 1, yjust = .5, bty = "n", xpd = TRUE)
        }
        axis(side = 1, at = labs, labels = labs, fg = fg)
        abline(h = c(0, alpha), col = c("grey", "blue"), lty = seq_len(2))
        par(mfrow = op$mfrow, mar = op$mar)
    }
    list(res = res, opt = opt, sel = sel)
}
