dkplsr <- function(X, Y, weights = NULL, nlv, kern = "krbf", ...) {
    
    kern <- eval(parse(text = kern))
    dots <- list(...)
    K <- kern(X, ...)
    fm <- plskern(K, Y, nlv = nlv)

    structure(
        list(X = X, fm = fm, K = K, kern = kern, dots = dots),
        class = c("Dkplsr", "Dkpls")
        )
    
    }

transform.Dkpls <- function(object, X, ..., nlv = NULL) {
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    transform(object$fm, K, nlv = nlv)
    }


coef.Dkpls <- function(object, ..., nlv = NULL) {
    z <- coef(object$fm, nlv = nlv)
    row.names(z$B) <- paste("i", seq_len(dim(z$B)[1]), sep = "")
    z
    }

predict.Dkplsr <- function(object, X, ..., nlv = NULL) {
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    pred <- predict(object$fm, K, nlv = nlv)$pred
    list(pred = pred, K = K)
    }
