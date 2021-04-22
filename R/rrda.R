rrda <- function(X, y, lb = 0, weights = NULL) {
    if(is.factor(y))
        y <- as.character(y)
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    z <- dummy(y)
    fm <- rr(X, z$Y, lb = lb, weights = weights)
    structure(
        list(V = fm$V, tTDY = fm$tTDY, eig = fm$eig, lb = lb,
            xmeans = fm$xmeans, ymeans = fm$ymeans, weights = fm$weights, 
            lev = z$lev, ni = z$ni),
        class = c("Rrda", "Rr")
        )   
    }

predict.Rrda <- function(object, X, ..., lb = NULL) {
    X <- .mat(X)
    q <- length(object$ymeans)
    rownam <- row.names(X)
    colnam <- "y1"
    if(is.null(lb))
        lb <- object$lb
    le_lb <- length(lb)
    posterior <- pred <- vector(mode = "list", length = le_lb)
    for(i in seq_len(le_lb)) {
        z <- coef(object, lb = lb[i])
        zposterior <- t(c(z$int) + t(X %*% z$B))        
        z <- apply(zposterior, FUN = .findmax, MARGIN = 1)
        zpred <- matrix(.replace_bylev(z, object$lev), ncol = 1)
        dimnames(zpred) <- list(rownam, colnam)
        pred[[i]] <- zpred
        posterior[[i]] <- zposterior        
        }
    names(posterior) <- names(pred) <- paste("lv", lb, sep = "")
    if(le_lb == 1) {
        pred <- pred[[1]] 
        posterior <- posterior[[1]]
        }
    list(pred = pred, posterior = posterior)
    }
    
