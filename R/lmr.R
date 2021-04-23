lmr <- function(X, Y, weights = NULL) {
    fm <- lm(Y ~ X, weights = weights)
    structure(
        fm,
        class = c("Lmr")
        )
    }

coef.Lmr <- function(object, ...) {
    z <- .mat(object$coefficients, "y")
    int <- z[1, ]
    B <- z[-1, , drop = FALSE]
    row.names(B) <- paste("x", seq(dim(B)[1]), sep = "")
    list(int = int, B = B) 
    }

predict.Lmr <- function(object, X, ...) {
    X <- .mat(X)
    rownam <- row.names(X)
    z <- coef(object)
    pred <- t(c(z$int) + t(X %*% z$B))
    list(pred = pred)
    }
