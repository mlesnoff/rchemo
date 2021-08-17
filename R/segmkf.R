segmkf <- function(n, y = NULL, K = 5, 
    type = c("random", "consecutive", "interleaved"), nrep = 1) {
    
    type <- match.arg(type)

    segm <- vector("list", length = nrep)
    names(segm) <- paste("rep", seq_len(nrep), sep = "")
    
    n <- round(n)
    
    zn <- n
    if(!is.null(y)) {
        if(length(y) != n) stop("y must be of size n")
        yagg <- unique(y)
        zn <- length(yagg)
        }

    lseg <- ceiling(zn / K)
    nna <- K * lseg - zn
    
    for(i in seq_len(nrep)) {
    
        z <- switch(type,
            random = matrix(c(sample(seq_len(zn)), rep(NA, nna)), ncol = K, byrow = TRUE),
            consecutive = {
                x <- c(matrix(c(rep(1, zn), rep(NA, nna)), ncol = K, byrow = TRUE))
                x[!is.na(x)] <- cumsum(na.omit(x))
                x <- matrix(x, ncol = K, byrow = FALSE)
                x
                },
            interleaved = matrix(c(seq_len(zn), rep(NA, nna)), ncol = K, byrow = TRUE)
            )
        z <- lapply(data.frame(z), FUN = function(x) c(na.omit(x)))
        names(z) <- paste("segm", seq_len(K), sep = "")
        
        segm[[i]] <- z
        
        }
    
    if(!is.null(y)) {
        zsegm <- segm
        for(i in seq_len(nrep)) {
            for(j in seq_len(K)){
                u <- segm[[i]][[j]]
                v <- which(y %in% yagg[u])
                zsegm[[i]][[j]] <- v
                }
            }
        segm <- zsegm    
        }

    segm
    
    }
