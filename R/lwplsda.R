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
        class = "Lwplsda"
        )

    }
    
    
predict.Lwplsda <- function(object, X, ..., nlv = NULL)
    .predict_Lwpls(object, X, ..., nlv = nlv, fun = plsda)
   

