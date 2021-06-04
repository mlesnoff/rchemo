plsrda_agg <- function(X, y, weights = NULL, nlv) {
    nlv <- eval(parse(text = nlv))
    fm <- plsrda(X, y, weights = weights, nlv = max(nlv))
    structure(list(fm = fm, nlv = nlv), 
              class = "Plsda_agg")
}

.plsprobda_agg <- function(X, y, weights = NULL, nlv, 
                           fun, prior = c("unif", "prop")) {
    nlv <- eval(parse(text = nlv))
    prior <- match.arg(prior)
    fm <- fun(X, y, weights = weights, nlv = max(nlv), prior = prior)
    structure(list(fm = fm, nlv = nlv), 
              class = "Plsda_agg")
}
plslda_agg <- function(X, y, weights = NULL, nlv, prior = c("unif", "prop"))
    .plsprobda_agg(X, y, weights, nlv, fun = plslda, prior = prior)
plsqda_agg <- function(X, y, weights = NULL, nlv, prior = c("unif", "prop"))
    .plsprobda_agg(X, y, weights, nlv, fun = plsqda, prior = prior)

predict.Plsda_agg <- function(object, X, ...) {
    X <- .mat(X)
    m <- dim(X)[1]
    nlv <- object$nlv  
    zpred <- predict(object$fm, X, nlv = nlv)$pred
    le_nlv <- length(nlv)
    if(le_nlv == 1)
        pred <- zpred
    else {
      pred <- zpred[[1]]
      z <- array(dim = c(m, le_nlv))
      for(j in seq(le_nlv))
        z[, j] <- zpred[[j]]
      for(i in seq_len(m)) {
          u <- z[i, ]
          lev <-  sort(unique(u))
          cnt <- c(table(u))
          pred[i, 1] <- lev[.findmax(cnt)]
      }
      }
    list(pred = pred, predlv = zpred)
}
