wdist <- function(d, h, cri = 4, squared = FALSE) {
    
    d <- c(d)
    if(squared)
        d <- d^2

    zmed <-  median(d)
    zmad <- mad(d)
    w <- ifelse(d < zmed + cri * zmad, exp(-d / (h * zmad)), 0)
    w <- w / max(w)
    
    w[is.na(w) | is.nan(w)] <- 1
        
    w
    
    }





