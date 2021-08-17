\name{gridcv}
\alias{gridcv}
\alias{gridcvlv}
\alias{gridcvlb}

\encoding{latin1}

\title{Cross-validation}

\description{

Functions for cross-validating predictive models.

The functions return "scores" (average error rates) of predictions for a given model and a grid of parameter values, calculated from a cross-validation process. 
 
- \code{gridcv}: Can be used for any model. 

- \code{gridcvlv}: Specific to models using regularization by latent variables (LVs) (e.g. PLSR). Much faster than \code{gridcv}.

- \code{gridcvlb}: Specific to models using ridge regularization (e.g. RR). Much faster than \code{gridcv}.

Argument \code{pars} (the grid) must be a list of named vectors, each vector corresponding to an argument of the model function and giving the parameter values to consider for this argument. This list can eventually be built with function \code{\link{mpars}}, which returns all the combinations of the input parameters, see the examples. 

For \code{gridcvlv}, \code{pars} must not contain \code{nlv} (nb. LVs), and for \code{gridcvlb}, \code{lb} (regularization parameter \eqn{lambda}).

}

\usage{

gridcv(X, Y, segm, score, fun, pars, verb = TRUE)

gridcvlv(X, Y, segm, score, fun, nlv, pars = NULL, verb = TRUE)

gridcvlb(X, Y, segm, score, fun, lb, pars = NULL, verb = TRUE)

}

\arguments{

\item{X}{Training X-data (\eqn{n, p}).}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{segm}{CV segments, typically output of \code{\link{segmkf}} or \code{\link{segmts}}.}

\item{score}{A function calculating a prediction score (e.g. \code{\link{msep}}).}

\item{fun}{A function corresponding to the predictive model.}

\item{nlv}{For \code{gridcvlv}. A vector of numbers of LVs.}

\item{lb}{For \code{gridcvlb}. A vector of ridge regulariation parameters.}

\item{pars}{A list of named vectors.  Each vector must correspond to an argument of the model function and gives the parameter values to consider for this argument.}

\item{verb}{Logical. If \code{TRUE}, fitting information are printed.}

}

\value{Dataframes with the prediction scores for the grid.}

\examples{

######################## EXAMPLES WITH PLSR

n <- 50 ; p <- 8
X <- matrix(rnorm(n * p), ncol = p)
y <- rnorm(n)
Y <- cbind(y, 10 * rnorm(n))

K = 3
segm <- segmkf(n = n, K = K, nrep = 1)
segm

## Using gridcv
nlv <- 5
pars <- mpars(nlv = 1:nlv)
pars
gridcv(
    X, Y, segm,
    score = msep, 
    fun = plskern, 
    pars = pars, verb = TRUE)

## Using gridcvlv (much faster) 
gridcvlv(
    X, Y, segm, 
    score = msep, 
    fun = plskern, 
    nlv = 0:nlv, verb = TRUE)

######################## EXAMPLES WITH PLSLDA

n <- 50 ; p <- 8
X <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
y <- sample(c(1, 4, 10), size = n, replace = TRUE)
#y <- sample(c("a", "10", "d"), size = n, replace = TRUE)
#y <- as.factor(sample(c(1, 4, 10), size = n, replace = TRUE))
#y <- as.factor(sample(c("a", "10", "d"), size = n, replace = TRUE))

K = 3
segm <- segmkf(n = n, K = K, nrep = 1)
segm

## Using gridcv
nlv <- 5
pars <- mpars(nlv = 1:nlv, prior = c("unif", "prop"))
pars
gridcv(
    X, y, segm, 
    score = err, 
    fun = plslda,
    pars = pars, verb = TRUE)

## Using gridcvlv (much faster) 
pars <- mpars(prior = c("unif", "prop"))
pars
gridcvlv(
    X, y, segm, 
    score = err, 
    fun = plslda,
    nlv = 1:nlv, pars = pars, verb = TRUE)

######################## EXAMPLES WITH RR

n <- 50 ; p <- 8
X <- matrix(rnorm(n * p), ncol = p)
y <- rnorm(n)
Y <- cbind(y, 10 * rnorm(n))

K = 3
segm <- segmkf(n = n, K = K, nrep = 1)
segm

## Using gridcv
lb <- c(.1, 1)
pars <- mpars(lb = lb)
pars
gridcv(
    X, Y, segm, 
    score = msep, 
    fun = rr, 
    pars = pars, verb = TRUE)

## Using gridcvlb (much faster) 
gridcvlb(
    X, Y, segm, 
    score = msep, 
    fun = rr, 
    lb = lb, verb = TRUE)

######################## EXAMPLES WITH KRR

n <- 50 ; p <- 8
X <- matrix(rnorm(n * p), ncol = p)
y <- rnorm(n)
Y <- cbind(y, 10 * rnorm(n))

K = 3
segm <- segmkf(n = n, K = K, nrep = 1)
segm

## Using gridcv
lb <- c(.1, 1)
gamma <- 10^(-1:1)
pars <- mpars(lb = lb, gamma = gamma)
pars
gridcv(
    X, Y, segm, 
    score = msep, 
    fun = krr, 
    pars = pars, verb = TRUE)

## Using gridcvlb (much faster) 
pars <- mpars(gamma = gamma)
gridcvlb(
    X, Y, segm, 
    score = msep, 
    fun = krr, 
    lb = lb, pars = pars, verb = TRUE)

######################## EXAMPLES WITH LWPLSR

n <- 50 ; p <- 8
X <- matrix(rnorm(n * p), ncol = p)
y <- rnorm(n)
Y <- cbind(y, 10 * rnorm(n))

K = 3
segm <- segmkf(n = n, K = K, nrep = 1)
segm

nlvdis <- 5
h <- c(1, Inf)
k <- c(10, 20)
nlv <- 5
pars <- mpars(nlvdis = nlvdis, diss = "mahal",
              h = h, k = k)
pars
res <- gridcvlv(
    X, Y, segm, 
    score = msep, 
    fun = lwplsr, 
    nlv = 0:nlv, pars = pars, verb = TRUE)
res

}

