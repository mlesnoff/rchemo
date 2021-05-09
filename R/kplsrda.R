kplsrda <- function(X, y, weights = NULL, nlv, kern = "krbf", ...) {
    if(is.factor(y))
        y <- as.character(y)
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    z <- dummy(y)
    fm <- kplsr(X, z$Y, nlv = nlv, kern = kern, weights = weights, ...)
    structure(
        list(fm = fm, lev = z$lev, ni = z$ni),
        class = c("Kplsrda"))   
    }

predict.Kplsrda <- function(object, X, ..., nlv = NULL) {
    X <- .mat(X)
    q <- length(object$fm$ymeans)
    rownam <- row.names(X)
    colnam <- "y1"
    a <- dim(object$fm$T)[2]
    if(is.null(nlv))
        nlv <- a 
    else 
        nlv <- seq(min(nlv), min(max(nlv), a))        
    le_nlv <- length(nlv)
    posterior <- pred <- vector(mode = "list", length = le_nlv)
    for(i in seq_len(le_nlv)) {
        zposterior <- predict(object$fm, X, nlv = nlv[i])$pred        
        z <- apply(zposterior, FUN = .findmax, MARGIN = 1)
        zpred <- matrix(.replace_bylev(z, object$lev), ncol = 1)
        dimnames(zpred) <- list(rownam, colnam)
        pred[[i]] <- zpred
        posterior[[i]] <- zposterior        
        }
    names(posterior) <- names(pred) <- paste("lv", nlv, sep = "")
    if(le_nlv == 1) {
        pred <- pred[[1]] 
        posterior <- posterior[[1]]
        }
    list(pred = pred, posterior = posterior)
    }
    
