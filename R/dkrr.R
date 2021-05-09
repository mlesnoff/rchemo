dkrr <- function(X, Y, weights = NULL, lb = 1e-2, kern = "krbf", ...) {
    kern <- eval(parse(text = kern))
    dots <- list(...)
    K <- kern(X, ...)
    fm <- rr(K, Y, lb = lb)
    structure(
        list(X = X, fm = fm, K = K, kern = kern, dots = dots),
        class = c("Dkrr")
        )
    }

coef.Dkrr <- function(object, ..., lb = NULL) {
    z <- coef(object$fm, lb = lb)
    row.names(z$B) <- paste("i", seq_len(dim(z$B)[1]), sep = "")
    z
    }

predict.Dkrr <- function(object, X, ..., lb = NULL) {
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    pred <- predict(object$fm, K, lb = lb)$pred
    list(pred = pred, K = K)
    }
