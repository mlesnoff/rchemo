knnda <- function(X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k) {
    structure(
        list(X = X, y = y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k),
        class = "Knnda"
        )
    }

predict.Knnda <- function(object, X, ...) {
    X <- .mat(X)
    m <- dim(X)[1]
    if(is.factor(object$y))
        object$y <- as.character(object$y)
    ## Getknn
    if (object$nlvdis == 0)
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
    else {
        fm <- plskern(object$X, dummy(object$y)$Y, nlv = object$nlvdis)
        res <- getknn(fm$T, transform(fm, X), k = object$k, diss = object$diss)
        }
    ## End
    listw <- lapply(res$listd, wdist, h = object$h)
    pred <- matrix(nrow = m, ncol = 1)
    for(i in seq_len(m)) { 
        s <- res$listnn[[i]]
        zy <- object$y[s]
        w <- .mweights(listw[[i]])
        dat <- data.frame(y = zy, w = w, stringsAsFactors = FALSE)
        cnt <- dtagg(w ~ y, FUN = sum, data = dat)
        ind <- .findmax(cnt$w)
        pred[i, ] <- cnt$y[ind]
        }
    dimnames(pred) <- list(row.names(X), "y1")
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
    }




