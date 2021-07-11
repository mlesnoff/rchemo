residreg <- function(pred, Y) {
    r <- Y - pred
    .mat(r,  "y")
    }

residcla <- function(pred, y) {
    if(is.factor(y))
        y <- as.character(y)
    r <- as.numeric(pred != y)
    .mat(r,  "y")
    }

msep <- function(pred, Y) {
    r <- residreg(pred, Y)
    colMeans(r^2)
    }

rmsep <- function(pred, Y)
    sqrt(msep(pred, Y))


bias <- function(pred, Y) {
    r <- residreg(pred, Y)
    -colMeans(r)
    }

sep <- function(pred, Y)
    sqrt(msep(pred, Y) - bias(pred, Y)^2)
    
r2 <- function(pred, Y) {
    Y <- .mat(Y)
    z <- .scale(Y / Y, scale = 1 / colMeans(Y))
    #z <- t(t(Y / Y) * colMeans(Y))
    1 - msep(pred, Y) / msep(z, Y)
    }

cor2 <- function(pred, Y) {
    z <- cor(pred, Y)^2
    if(is.matrix(z))
        z <- diag(z)
    z
    }

rpd <- function(pred, Y) {
    Y <- .mat(Y)
    n <- dim(Y)[1]
    apply(.mat(Y), FUN = function(x) sd(x) * sqrt((n - 1) / n),
        MARGIN = 2) / rmsep(pred, Y) 
    }

rpq <- function(pred, Y) {
    apply(.mat(Y), FUN = IQR, MARGIN = 2) / rmsep(pred, Y) 
    }

err <- function(pred, y) {
    r <- residcla(pred, y)
    sum(r) / dim(r)[1]
    }

mse <- function(pred, Y, digits = 3) {
    res <- data.frame(
        msep  = msep(pred, Y),
        rmsep = sqrt(msep(pred, Y)),
        sep   = sep(pred, Y),
        bias  = bias(pred, Y),
        cor2  = cor2(pred, Y),        
        r2    = r2(pred, Y),        
        rpd = rpd(pred, Y),
        rpq = rpq(pred, Y),
        mean  = .colmeans(Y)
        )
    round(res, digits = digits)
    }

        
        
        

    
    
    
    
    

