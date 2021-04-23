lwplsrda <- function(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv
    ) {
    structure(
        list(X = X, y = y,
             nlvdis = nlvdis, diss = diss, 
             h = h, k = k, nlv = nlv),
        class = "Lwplsrda"
        )
    }
    
predict.Lwplsrda <- function(object, X, ..., nlv = NULL)
    .predict_lwpls(object, X, ..., nlv = nlv, fun = plsrda)
   

