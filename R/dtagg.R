dtagg <- function(formula, data, FUN = mean, ...) {
    
    f <- formula
    
    z <- data.table(data)
    
    rvar <- attr(terms(f), "term.labels")
    vars <- all.vars(f)
    lvar <- vars[!(vars %in% rvar)]

    z <- z[, by = rvar, FUN(eval(parse(text = lvar)), ...)]
    
    if(length(rvar) > 0)
        z <- setorderv(z, rvar)
    
    z <- setDF(z)
    names(z) <- c(rvar, lvar)
    
    z

    }