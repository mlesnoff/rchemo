svmr <- function(X, y, C = 1, epsilon = .1, sigma = 1, scale = FALSE) {
    fm <- e1071::svm(
        X, y,
        cost = C, epsilon = epsilon,
        kernel = "radial", 
        gamma = 1 / (2 * sigma^2),
        scale = scale,
        type = "eps-regression", shrinking = TRUE)
    fm$isnum <- FALSE
    structure(list(fm = fm), class = "Svm")
}

svmda <- function(X, y, C = 1, epsilon = .1, sigma = 1, scale = FALSE) {
    fm <- e1071::svm(
        X, as.factor(y),
        cost = C, epsilon = epsilon,
        kernel = "radial", 
        gamma = 1 / (2 * sigma^2),
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