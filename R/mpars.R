mpars <- function(...) {
    dots <- list(...)
    if(length(names(dots)) == 1)
        res <- dots
    else {
        z <- expand.grid(dots)
        res <- lapply(z, FUN = function(x) {if(is.factor(x)) as.character(x) else x})
        }
    res
    }
