krrda <- function(X, y, weights = NULL, lb = 1e-5, kern = "krbf", ...) {
    if(is.factor(y))
        y <- as.character(y)
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    z <- dummy(y)
    fm <- krr(X, z$Y, lb = lb, kern = kern, weights = weights, ...)
    structure(
        list(fm = fm, lev = z$lev, ni = z$ni),
        class = c("Krrda")
        )   
    }

predict.Krrda <- function(object, X, ..., lb = NULL) {
    X <- .mat(X)
    q <- length(object$fm$ymeans)
    rownam <- row.names(X)
    colnam <- "y1"
    if(is.null(lb))
        lb <- object$fm$lb
    le_lb <- length(lb)
    posterior <- pred <- vector(mode = "list", length = le_lb)
    for(i in seq_len(le_lb)) {
        zposterior <- predict(object$fm, X, lb = lb[i])$pred        
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
    
