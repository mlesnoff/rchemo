lmrda <- function(X, y, weights = NULL) {
    if(is.factor(y))
        y <- as.character(y)
    X <- .mat(X)
    n <- dim(X)[1]
    if(is.null(weights))
        weights <- rep(1, n)
    weights <- .mweights(weights)
    z <- dummy(y)
    fm <- lmr(X, z$Y, weights = weights)
    structure(
        list(fm = fm, lev = z$lev, ni = z$ni),
        class = c("Lmrda")
        )   
    }

predict.Lmrda <- function(object, X, ...) {
    X <- .mat(X)
    q <- length(object$fm$ymeans)
    rownam <- row.names(X)
    colnam <- "y1"
    posterior <- predict(object$fm, X)$pred
    z <- apply(posterior, FUN = .findmax, MARGIN = 1)
    pred <- matrix(.replace_bylev(z, object$lev), ncol = 1)
    dimnames(pred) <- list(rownam, colnam)
    list(pred = pred, posterior = posterior)
    }
    






