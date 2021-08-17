segmts <- function(n, y = NULL, m, nrep = 1) {
    
    segm <- vector("list", length = nrep)
    names(segm) <- paste("rep", seq_len(nrep), sep = "")
    
    n <- round(n)
    m <- round(m)
    
    zn <- n
    if(!is.null(y)) {
        if(length(y) != n) stop("y must be of size n.")
        yagg <- unique(y)
        zn <- length(yagg)
        }
    
    for(i in seq_len(nrep)) {
        z <- sample(seq_len(zn), size = m, replace = FALSE)
        z <- list(z)
        names(z) <- "segm1"
        segm[[i]] <- z
        }
    
    if(!is.null(y)) {
        zsegm <- segm
        for(i in seq_len(nrep)) {
            u <- segm[[i]][[1]]
            v <- which(y %in% yagg[u])
            zsegm[[i]][[1]] <- v
            }
        segm <- zsegm    
        }

    segm
    
    }

