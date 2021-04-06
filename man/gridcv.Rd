\name{gridcv}
\alias{gridcv}
\alias{gridcv_lv}

\encoding{latin1}

\title{Cross-validation}

\description{

Wrappers for cross-validating predictive models.

The functions return prediction scores (in general error rates) for a given model, a grid of parameter values and a cross-validation process. 
 
Function \code{gridcv} can be used for any model. Function \code{gridcv_lv} is specific to models based on latent variables (e.g. PLSR) for which it is much faster than \code{gridscore} and recommended.

The grid (argument \code{pars}) must be a dataframe whose the column names correspond to the model parameters involved in the score calculations. For \code{gridcv_lv}, the grid must contain a column \code{nlv} (number of latent variables; LVs).   

}

\usage{

gridcv(X, Y, segm, score, fun, pars, verb = TRUE)

gridcv_lv(X, Y, segm, score, fun, pars, verb = TRUE)

}

\arguments{

\item{X}{Training X-data (\eqn{n, p}).}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{segm}{CV segments, typically output of \code{\link{segmkf}} or \code{\link{segmts}}.}

\item{score}{A function calculating a prediction score (e.g. \code{\link{msep}}).}

\item{fun}{A function corresponding to the predictive model.}

\item{pars}{A dataframe defining the parameter values (= the grid).}

\item{verb}{Logical. If \code{TRUE}, fitting information are printed.}

}

\value{

A dataframe with the prediction scores for the grid.

}

\examples{

############## EXAMPLES WITH PLSR

n <- 50 ; p <- 10
set.seed(seed = 1)
X <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
y <- rnorm(n)
Y <- cbind(y, 10 * rnorm(n))
set.seed(seed = NULL)

K = 3
segm <- segmkf(n = n, K = K, nrep = 1)

nlv <- 5
pars <- data.frame(nlv = 1:nlv)
gridcv(
    X, Y, 
    segm, score = msep, fun = plskern, 
    pars = pars
    )
gridcv_lv(
    X, Y, 
    segm, score = msep, fun = plskern, 
    pars = pars
    )

############## EXAMPLES WITH PLSLDA

n <- 50 ; p <- 30
X <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
y <- sample(c(1, 4, 10), size = n, replace = TRUE)

K = 5
segm <- segmkf(n = n, K = K, nrep = 1)

nlv <- 4
pars <- data.frame(nlv = 1:nlv)
gridcv(
    X, y, 
    segm, score = err, fun = plslda, 
    pars
    )
gridcv_lv(
    X, y, 
    segm, score = err, fun = plslda, 
    pars
    )

pars <- expand.grid(
    nlv = 1:nlv,
    prior = c("unif", "prop"),
    stringsAsFactors = FALSE
    )
pars
gridcv(
    X, y, 
    segm, score = err, fun = plslda, 
    pars
    )
gridcv_lv(
    X, y, 
    segm, score = err, fun = plslda, 
    pars
    )



}

