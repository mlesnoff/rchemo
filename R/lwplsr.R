lwplsr <- function(
    X, Y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv
    ) {

    structure(
        list(X = X, Y = Y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv),
        class = "LwPls"
        )

    }
    
    
predict.LwPls <- function(fm, X, ..., nlv = NULL) {
    
    X <- .mat(X)

    A <- fm$nlv
    if(is.null(nlv))
        nlv <- A 
    else 
        nlv <- seq(min(nlv), min(max(nlv), A))
    le_nlv <- length(nlv)
    
    ## Getknn
    if (fm$nlvdis == 0)
        res <- getknn(fm$X, X, k = fm$k, diss = fm$diss)
    else {
        zfm <- plskern(fm$X, fm$Y, fm$nlvdis)
        res <- getknn(zfm$T, transform(zfm, X), k = fm$k, diss = fm$diss)
        }
    ## End
    listw <- lapply(res$listd, wdist, h = fm$h)    

    pred <- locwlv(fm$X, fm$Y, X,
        listnn = res$listnn, listw = res$listw, fun = plskern, nlv = nlv)$pred

    list(pred = pred, listnn = res$listnn, listd = res$listd, listw = listw)
    
    }


