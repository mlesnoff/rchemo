summ <- function(X, nam = NULL, digits = 3){

    X <- as.data.frame(X)
    if(is.null(nam)) nam <- names(X)
    X <- X[, nam]
    if(is.vector(X)) {X <- data.frame(X) ; names(X) <- nam}
    
    n <- nrow(X)
    
    z <- lapply(X, FUN = function(x) is.numeric(x))
    z <- which(z == TRUE)
    if(length(z) == 0) stop("No numeric variable(s) in the data.")
        else {
            nam <- nam[z]
            X <- data.frame(X[, z]) 
            names(X) <- nam
            }
    
    for(j in seq_len(length(nam))) {
        
        z <- X[, nam[j]]
        if(is.numeric(z)) {
            z <- z[!is.na(z)]
            v <- c(NbVal = length(z), summary(z), Stdev = sd(z), NbNA = n - length(z))
            } else
                v <- c(rep(NA, 9), 0)
                
        if(j == 1) tab <- v else tab <- rbind(tab, v)
        
        }
    tab <- round(tab, digits = digits)
    if(is.vector(tab)) tab <- as.matrix(t(tab))
    tab <- data.frame(Name = nam, tab)
    nam <- c("Name", "NbVal", "Mean", "Min.", "Max.","Stdev", 
        "Median", "X1st.Qu.", "X3rd.Qu.", "NbNA")
    tab <- tab[, nam]
    row.names(tab) <- seq_len(nrow(tab))
        
    list(tab = tab, ntot = n)

    }
 











