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

# Mean of the squared prediction errors
msep <- function(pred, Y) {
    r <- residreg(pred, Y)
    colMeans(r^2)
}

# Square root of the mean of the squared prediction errors
rmsep <- function(pred, Y)
    sqrt(msep(pred, Y))

# Prediction bias, i.e. opposite of the mean of the prediction errors
bias <- function(pred, Y) {
    r <- residreg(pred, Y)
    -colMeans(r)
}

# SEP = SEP_c = Standard deviation of the prediction errors
sep <- function(pred, Y)
    sqrt(msep(pred, Y) - bias(pred, Y)^2)

# Squared linear correlation coefficient    
cor2 <- function(pred, Y) {
    z <- cor(pred, Y)^2
    if(is.matrix(z))
        z <- diag(z)
    z
}

# R2
r2 <- function(pred, Y) {
    Y <- .mat(Y)
    z <- .scale(Y / Y, scale = 1 / colMeans(Y))
    #z <- t(t(Y / Y) * colMeans(Y))
    1 - msep(pred, Y) / msep(z, Y)
}

# RPD = SD(Y) / RMSEP 
#     = RMSEP(null model) / RMSEP
# Ratio between the deviation: sqrt of the mean of the squared prediction errors of the null model (simple average)
# and the performance : sqrt of the mean of the squared prediction errors of the model
rpd <- function(pred, Y) {
    Y <- .mat(Y)
    n <- dim(Y)[1]
    apply(.mat(Y), FUN = function(x) sd(x) * sqrt((n - 1) / n),
        MARGIN = 2) / rmsep(pred, Y) 
}

# Robust RPD = MAD(Y) / Median(Abs(prediction errors))
# MAD = Median(Abs(prediction errors)) where the median is the null model
rpdr <- function(pred, Y) {
    u <- apply(.mat(Y), FUN = mad, MARGIN = 2) / 1.4826
    r <- residreg(pred, Y)
    v <- apply(.mat(abs(r)), FUN = median, MARGIN = 2)
    u / v
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
        rpdr = rpdr(pred, Y),
        mean  = .colmeans(Y)
        )
    round(res, digits = digits)
    }

err <- function(pred, y) {
    r <- residcla(pred, y)
    sum(r) / dim(r)[1]
    }


        
        

    
    
    
    
    

