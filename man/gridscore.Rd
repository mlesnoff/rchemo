\name{gridscore}
\alias{gridscore}
\alias{gridscorelv}
\alias{gridscorelb}
\alias{mpars}

\encoding{latin1}

\title{Tuning of predictive models on a validation dataset}

\description{

Functions for tuning predictive models on a validation set.

The functions return "scores" (average error rates) of predictions for a given model and a grid of parameter values, calculated on a validation dataset. 
 
- \code{gridscore}: Can be used for any model.

- \code{gridscorelv}: Specific to models using regularization by latent variables (LVs) (e.g. PLSR). Much faster than \code{gridscore}.

- \code{gridscorelb}: Specific to models using ridge regularization (e.g. RR). Much faster than \code{gridscore}.

Argument \code{pars} (the grid) must be a list of named vectors, each vector corresponding to an argument of the model function and giving the parameter values to consider for this argument. This list can eventually be built with function \code{mpars}, which returns all the combinations of the input parameters, see the examples. 

For \code{gridscorelv}, \code{pars} must not contain \code{nlv} (nb. LVs), and for \code{gridscorelb}, \code{lb} (regularization parameter \eqn{lambda}).

}

\usage{

gridscore(Xtrain, Ytrain, X, Y, score, fun, pars, verb = FALSE)

gridscorelv(Xtrain, Ytrain, X, Y, score, fun, nlv, pars = NULL, verb = FALSE)

gridscorelb(Xtrain, Ytrain, X, Y, score, fun, lb, pars = NULL, verb = FALSE)

}

\arguments{

\item{Xtrain}{Training X-data (\eqn{n, p}).}

\item{Ytrain}{Training Y-data (\eqn{n, q}).}

\item{X}{Validation X-data (\eqn{n, p}).}

\item{Y}{Validation Y-data (\eqn{n, q}).}

\item{score}{A function calculating a prediction score (e.g. \code{\link{msep}}).}

\item{fun}{A function corresponding to the predictive model.}

\item{nlv}{For \code{gridscorelv}. A vector of numbers of LVs.}

\item{lb}{For \code{gridscorelb}. A vector of ridge regulariation parameters.}

\item{pars}{A list of named vectors.  Each vector must correspond to an argument of the model function and gives the parameter values to consider for this argument.}

\item{verb}{Logical. If \code{TRUE}, fitting information are printed.}

}

\value{A dataframe with the prediction scores for the grid.}

\examples{

######################## EXAMPLES WITH PLSR

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
ytrain <- rnorm(n)
Ytrain <- cbind(ytrain, 10 * rnorm(n))
m <- 3
Xtest <- Xtrain[1:m, ] 
Ytest <- Ytrain[1:m, ] ; ytest <- Ytest[, 1]

## Using gridscore
nlv <- 5
pars <- mpars(nlv = 1:nlv)
pars
gridscore(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = plskern, 
    pars = pars, verb = TRUE
    )

## Using gridscorelv (much faster) 
gridscorelv(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = plskern, 
    nlv = 0:nlv, verb = TRUE
    )

fm <- plskern(Xtrain, Ytrain, nlv = nlv)
pred <- predict(fm, Xtest)$pred
msep(pred, Ytest)

######################## EXAMPLES WITH PLSLDA

n <- 50 ; p <- 8
X <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
y <- sample(c(1, 4, 10), size = n, replace = TRUE)
#y <- sample(c("a", "10", "d"), size = n, replace = TRUE)
#y <- as.factor(sample(c(1, 4, 10), size = n, replace = TRUE))
#y <- as.factor(sample(c("a", "10", "d"), size = n, replace = TRUE))
Xtrain <- X ; ytrain <- y
m <- 5
Xtest <- X[1:m, ] ; ytest <- y[1:m]

## Using gridscore
nlv <- 5
pars <- mpars(nlv = 1:nlv, prior = c("unif", "prop"))
pars
gridscore(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, 
    fun = plslda,
    pars = pars, verb = TRUE
    )

fm <- plslda(Xtrain, ytrain, nlv = nlv)
pred <- predict(fm, Xtest)$pred
err(pred, ytest)

## Using gridscorelv (much faster) 
pars <- mpars(prior = c("unif", "prop"))
pars
gridscorelv(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, 
    fun = plslda,
    nlv = 1:nlv, pars = pars, verb = TRUE
    )

######################## EXAMPLES WITH RR

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
ytrain <- rnorm(n)
Ytrain <- cbind(ytrain, 10 * rnorm(n))
m <- 3
Xtest <- Xtrain[1:m, ] 
Ytest <- Ytrain[1:m, ] ; ytest <- Ytest[, 1]

## Using gridscore
lb <- c(.1, 1)
pars <- mpars(lb = lb)
pars
gridscore(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = rr, 
    pars = pars, verb = TRUE
    )

## Using gridscorelb (much faster) 
gridscorelb(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = rr, 
    lb = lb, verb = TRUE
    )

######################## EXAMPLES WITH KRR

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
ytrain <- rnorm(n)
Ytrain <- cbind(ytrain, 10 * rnorm(n))
m <- 3
Xtest <- Xtrain[1:m, ] 
Ytest <- Ytrain[1:m, ] ; ytest <- Ytest[, 1]

## Using gridscore
lb <- c(.1, 1)
gamma <- 10^(-1:1)
pars <- mpars(lb = lb, gamma = gamma)
pars
gridscore(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = krr, 
    pars = pars, verb = TRUE
    )

## Using gridscorelb (much faster) 
pars <- mpars(gamma = gamma)
gridscorelb(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = krr, 
    lb = lb, pars = pars, verb = TRUE
    )

######################## EXAMPLES WITH LWPLSR

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
ytrain <- rnorm(n)
Ytrain <- cbind(ytrain, 10 * rnorm(n))
m <- 3
Xtest <- Xtrain[1:m, ] 
Ytest <- Ytrain[1:m, ] ; ytest <- Ytest[, 1]

nlvdis <- 5
h <- c(1, Inf)
k <- c(10, 20)
nlv <- 5
pars <- mpars(nlvdis = nlvdis, diss = "mahal",
    h = h, k = k)
pars
res <- gridscorelv(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = lwplsr, 
    nlv = 0:nlv, pars = pars, verb = TRUE
    )
res


}

