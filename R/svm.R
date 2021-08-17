svmr <- function(X, y, cost = 1, epsilon = .1, gamma = 1, scale = FALSE) {
    fm <- e1071::svm(
        X, y,
        cost = cost, epsilon = epsilon,
        kernel = "radial", gamma = gamma,
        scale = scale,
        type = "eps-regression", shrinking = TRUE)
    fm$isnum <- FALSE
    structure(list(fm = fm), class = "Svm")
}

svmda <- function(X, y, cost = 1, epsilon = .1, gamma = 1, scale = FALSE) {
    fm <- e1071::svm(
        X, as.factor(y),
        cost = cost, epsilon = epsilon,
        kernel = "radial", gamma = gamma,
        scale = scale,
        type = "C-classification", shrinking = TRUE)
    fm$isnum <- FALSE
    if(is.numeric(y))
        fm$isnum <- TRUE
    structure(list(fm = fm), class = "Svm")
}

predict.Svm <- function(object, X, ...) {
    pred <- predict(object$fm, X)
    if(object$fm$isnum)
        pred <- as.numeric(as.character(pred))
    pred <- .mat(pred, "y")
    list(pred = pred)
}

summary.Svm <- function(object, ...)
    summary(object$fm)