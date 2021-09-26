checkna <- function(X) {
    X <- .mat(X)
    n <- dim(X)[1]
    z <- rowSums(is.na(X))
    z <- data.frame(rownum = seq_len(n), rownam = row.names(X), nbna = z)
    z$all.na <- ifelse(z$nbna == ncol(X), TRUE, FALSE)
    z
    }
  
checkdupl <- function(X, Y = NULL, digits = NULL) {
    zX <- as.data.frame(.mat(X))
    if(is.null(Y))
        zY <- zX
    else
        zY <- as.data.frame(.mat(Y))
    n <- dim(zX)[1]
    m <- dim(zY)[1]
    nam <- colnames(zX)
    zX$rownum1 <- seq_len(n)
    zY$rownum2 <- seq_len(m)
    if(!is.null(digits)) {
        zX <- round(zX, digits = digits)
        zY <- round(zY, digits = digits)
    }
    z <- merge(zX, zY, by = nam, all = FALSE, allow.cartesian = TRUE)
    znam <- c("rownum1", "rownum2", nam)
    z <- z[, znam]
    if(is.null(Y)) {
        z <- z[z$rownum1 != z$rownum2, ]
        v <- z[, c("rownum1", "rownum2")]
        for(i in seq_len(dim(v)[1])) 
          v[i, ] <- sort(unlist(v[i, ]))
        z <- merge(unique(v), z, all = FALSE)
        }
    z <- z[order(z$rownum1), ]
    row.names(z) <- seq_len(dim(z)[1])
    if(dim(z)[1] == 0)
        z <- NULL
    z
    }





