.gridcv <- function(X, Y, segm, score, fun, pars, verb = TRUE, fgridscore) {
  
    Y <- .mat(Y, "y")
    
    pars <- data.frame(
      lapply(pars, FUN = function(x) {if(is.factor(x)) as.character(x) else x})
      )
    npar <- dim(pars)[1]
    nrep <- length(segm)
    res_rep <- vector("list", length = nrep)
       
    for(i in seq_len(nrep)) {
          
        if(verb)
            cat("/ rep=", i, " ", sep = "")   
      
        listsegm <- segm[[i]]
        nsegm <- length(listsegm)
        zres <- vector("list", length = nsegm)
        for(j in seq_len(nsegm)) {
            if(verb)
                cat("segm=", j, " ", sep = "")
            s <- sort(listsegm[[j]])
            zres[[j]] <- fgridscore(
                X[-s, , drop = FALSE], 
                Y[-s, , drop = FALSE],
                X[s, , drop = FALSE],
                Y[s, , drop = FALSE],
                score = score, fun = fun,
                pars = pars
                )
            }
        zres <- setDF(rbindlist(zres))
        zres <- cbind(
            rep = rep(i, nsegm * npar),
            segm = sort(rep(1:nsegm, npar)), 
            zres
            )
      
        res_rep[[i]] <- zres
  
        }
  
    res_rep <- setDF(rbindlist(res_rep))
    if(verb)
        cat("/ End. \n\n")    
    
    namy <- colnames(Y)
    nampar <- colnames(pars) 
    res <- aggregate(res_rep[, namy, drop = FALSE], 
                     by = res_rep[, nampar, drop = FALSE], FUN = mean)
    
    list(val = res, val_rep = res_rep)
    
    }

gridcv <- function(X, Y, segm, score, fun, pars, verb = TRUE)
    .gridcv(X, Y, segm, score, fun, pars, verb = TRUE, gridscore)
  
gridcvlv <- function(X, Y, segm, score, fun, pars, verb = TRUE)
    .gridcv(X, Y, segm, score, fun, pars, verb = TRUE, gridscorelv)

gridcvlb <- function(X, Y, segm, score, fun, pars, verb = TRUE)
    .gridcv(X, Y, segm, score, fun, pars, verb = TRUE, gridscorelb)





