checkna <- function(X) {
    X <- .mat(X)
    n <- dim(X)[1]
    z <- rowSums(is.na(X))
    z <- data.frame(rownum = seq_len(n), rownam = row.names(X), nbna = z)
    z$all.na <- ifelse(z$nbna == ncol(X), TRUE, FALSE)
    z
    }
  
checkdupl <- function(X, Y, nam = NULL, digits = NULL, check.all = FALSE) {
    X <- .mat(X)
    Y <- .mat(Y)
    n <- dim(X)[1]
    m <- dim(Y)[1]
    rownam.X <- row.names(X)
    rownam.Y <- row.names(Y)
    X <- as.data.frame(X)
    Y <- as.data.frame(Y)
  
    if(!is.null(nam)) 
        nam <- as.character(nam)
    else 
        nam <- names(X)
  
    u <- X[, nam]
    if(!is.null(digits)) 
        u <- round(u, digits = digits)
    u$rownum.X <- rep(seq_len(n))
    u$rownam.X <- rownam.X
    zref <- u
  
    u <- Y[, nam]
    if(!is.null(digits)) 
        u <- round(u, digits = digits)
    u$rownum.Y <- rep(seq_len(m))
    u$rownam.Y <- rownam.Y
    z <- u
  
    z <- merge(zref, z, by = nam, all = FALSE, allow.cartesian = TRUE)
    nam <- c(nam, "rownum.X", "rownum.Y", "rownam.X", "rownam.Y")
    z <- z[, nam]
    if(check.all)
        z$all.equal <- (rowSums(abs(X[z$rownum.X, ])) == rowSums(abs(Y[z$rownum.Y, ])))
    z <- z[order(z$rownum.X), ]
    z
    }

rmdupl <- function(X, nam = NULL, digits = NULL, check.all = FALSE) {
    X <- .mat(X)
    X <- as.data.frame(X)
    z <- checkdupl(X = X, Y = X, nam = nam, digits = digits, check.all = FALSE)
    u <- z[z$rownum.X != z$rownum.Y, ]
    s <- NULL
    if(nrow(u)) {
        v <- t(u[, c("rownum.X", "rownum.Y")])
        for(i in seq_len(ncol(v))) 
            v[, i] <- sort(v[, i])
        v <- t(v)
        s <- sort(unique(v[, 2]))
        X <- X[-s, ]
        }
    list(X = X, rm = s)
    }



