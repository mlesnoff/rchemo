scordis <- function(
    Xtrain,
    X = NULL,
    fun = pcaeigen,
    nlv = NULL, 
    rob = FALSE, alpha = .01
    ) {

    
    if(is.null(fm$Tr))
        names(fm)[which(names(fm) == "T")] <- "Tr"
    
    if(is.null(nlv))
        nlv <- dim(fm$Tr)[2]
    else 
        nlv <- min(nlv, dim(fm$Tr)[2])
    
    if(fm$T.ortho) {
        tt <- colSums(
            fm$weights * fm$Tr[, seq_len(nlv), drop = FALSE] * fm$Tr[, seq_len(nlv), drop = FALSE]
            )
        S <- diag(tt, nrow = nlv, ncol = nlv)
        }
    else 
        S <- NULL
    
    res <- dis(fm$Tr[, seq_len(nlv), drop = FALSE], fm$Tu[, seq_len(nlv), drop = FALSE], 
                         rep(0, nlv), "mahalanobis", S)
    
    dr <- res$dr

    d2 <- dr$d^2
    if(!rob) {
        mu <- mean(d2)   
        s2 <- var(d2)
        }
    else{
        mu <- median(d2)
        s2 <- mad(d2)^2
        }
    nu <- 2 * mu^2 / s2
    cutoff <- sqrt(mu / nu * qchisq(1 - alpha, df = nu))
    
    dr$dstand <- dr$d / cutoff
    dr$gh <- dr$d^2 / nlv
    
    du <- NULL
    if(!is.null(fm$Tu[, seq_len(nlv), drop = FALSE])) {
        du <- res$du
        du$dstand <- du$d / cutoff
        du$gh <- du$d^2 / nlv
        }

    list(dr = dr, du = du, cutoff = cutoff)
    
    }


