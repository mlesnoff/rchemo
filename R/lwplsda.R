lwplsda <- function(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv
    ) {

    structure(
        list(X = X, y = y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv),
        class = "LwPlsDa"
        )

    }
    
    
predict.LwPlsDa <- function(fm, X, ..., nlv = NULL)
    .predict_LwPls(fm, X, ..., nlv = nlv, fun = plsda)
   

