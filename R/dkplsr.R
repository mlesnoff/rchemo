dkplsr <- function(X, Y, nlv, kern = "krbf", weights = NULL, ...) {
    
    kern <- eval(parse(text = kern))
    dots <- list(...)
    K <- kern(X, ...)
    fm <- plskern(K, Y, nlv = nlv)

    structure(
        list(X = X, fm = fm, K = K, kern = kern, dots = dots),
        class = c("Dkpls")
        )
    
    }

predict.Dkpls <- function(object, X, ..., nlv = NULL) {
    
    K <- do.call(object$kern, c(list(X = X, Y = object$X), object$dots))
    pred <- predict(object$fm, K, nlv = nlv)$pred
    
    list(pred = pred, K = K)
    
    }
