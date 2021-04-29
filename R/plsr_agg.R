plsr_agg <- function(X, Y, nlv, weights = NULL) {
    fm <- plskern(X, Y, max(nlv), weights = weights)
    structure(
        list(fm = fm, nlv = nlv),
        class = "Plsragg"
        )
    }

predict.Plsragg <- function(object, X, ...) {
    nlv <- object$nlv  
    zpred <- predict(object$fm, X, nlv = nlv)$pred
    le_nlv <- length(nlv)
    if(le_nlv == 1)
        pred <- zpred
    else {
      z <- array(dim = c(dim(zpred[[1]]), le_nlv))
      for(i in seq(le_nlv))
        z[, , i] <- zpred[[i]]
      pred <- apply(z, MARGIN = c(1, 2), FUN = mean)
      dimnames(pred) <- dimnames(zpred[[1]])
      }
    list(pred = pred, predlv = zpred)
    }
