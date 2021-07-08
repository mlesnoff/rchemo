knnr <- function(X, Y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k
    ) {
    structure(
        list(X = X, Y = Y,
             nlvdis = nlvdis, diss = match.arg(diss), 
             h = h, k = k),
        class = "Knnr")
    }

predict.Knnr <- function(object, X, ...) {
    X <- .mat(X)
    m <- dim(X)[1]
    object$Y <- .mat(object$Y, "y")
    q <- dim(object$Y)[2]
    ## Getknn
    if (object$nlvdis == 0) {
        res <- getknn(object$X, X, k = object$k, diss = object$diss)
        print(object$X)
        print(object$diss)
        print(res$listd)        
        }
    else {
        fm <- plskern(object$X, object$Y, nlv = object$nlvdis)
        res <- getknn(fm$T, transform(fm, X), k = object$k, diss = object$diss)
        }

    ## End
    listw <- lapply(res$listd, wdist, h = object$h)  
    pred <- matrix(nrow = m, ncol = q)
    for(i in seq_len(m)) { 
        s <- res$listnn[[i]]
        zY <- object$Y[s, , drop = FALSE]
        pred[i, ] <- .colmeans(zY, weights = listw[[i]])
        }
    dimnames(pred) <- list(row.names(X), colnames(object$Y))
    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
    }




