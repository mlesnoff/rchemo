\name{plsrda_agg}
\alias{plsrda_agg}
\alias{plslda_agg}
\alias{plsqda_agg}
\alias{predict.Plsda_agg}
\encoding{latin1}

\title{PLSDA with aggregation of latent variables}

\description{

Ensemblist approach where the predictions are calculated by "averaging" the predictions of PLSDA models built with different numbers of latent variables (LVs). 

For instance, if argument \code{nlv} is set to \code{nlv = "5:10"}, the prediction for a new observation is the most occurent level (vote) over the predictions returned by the models with 5 LVS, 6 LVs, ... 10 LVs.

- \code{plsrda_agg}: use \code{\link{plsrda}}.

- \code{plslda_agg}: use \code{\link{plslda}}.

- \code{plsqda_agg}: use \code{\link{plsqda}}.

}

\usage{

plsrda_agg(X, y, weights = NULL, nlv)

plslda_agg(X, y, weights = NULL, nlv, prior = c("unif", "prop"))

plsqda_agg(X, y, weights = NULL, nlv, prior = c("unif", "prop"))

\method{predict}{Plsda_agg}(object, X, ...)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}). \bold{Note:} If \code{y} is a factor, it is replaced by a character vector.}

\item{weights}{Weights (\eqn{n, 1}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{nlv}{A character string such as "5:20" defining the range of the numbers of LVs to consider (here: the models with nb LVS = 5, 6, ..., 20 are averaged). Syntax such as "10" is also allowed (here: correponds to the single model with 10 LVs).}

\item{prior}{The prior probabilities of the classes. Possible values are "unif" (default; probabilities are set equal for all the classes) or "prop" (probabilities are set equal to the observed proportions of the classes in \code{y}).}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\examples{

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- sample(c(1, 4, 10, 2), size = n, replace = TRUE)
#ytrain <- sample(c("a", "10", "d"), size = n, replace = TRUE)
m <- 5
Xtest <- Xtrain[1:m, ] ; ytest <- ytrain[1:m]

############################# PLSRDA-AGG

nlv <- "2:5"
fm <- plsrda_agg(Xtrain, ytrain, nlv = nlv)
names(fm)
res <- predict(fm, Xtest)
names(res)
## Final predictions (after aggregation)
res$pred
err(res$pred, ytest)
## Intermediate predictions (Per nb. LVs)
res$predlv

## Gridscore and gridcv
## Here, there is no sense to use gridscorelv and gridcvlv 

pars <- mpars(nlv = c("1:3", "2:5"))
## Same as:
## pars <- list(nlv = c("1:3", "2:5"))
pars

res <- gridscore(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, 
    fun = plsrda_agg, 
    pars = pars)
res

segm <- segmkf(n = n, K = 3, nrep = 1)
res <- gridcv(
    Xtrain, ytrain, 
    segm, score = err, 
    fun = plslda_agg, 
    pars = pars,
    verb = TRUE)
res

############################# PLSLDA-AGG

nlv <- "2:5"
fm <- plslda_agg(Xtrain, ytrain, nlv = nlv, prior = "unif")
names(fm)
res <- predict(fm, Xtest)
names(res)
res$pred
err(res$pred, ytest)
res$predlv

pars <- mpars(nlv = c("1:3", "2:5"), prior = c("unif", "prop"))
pars
res <- gridscore(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, 
    fun = plslda_agg, 
    pars = pars)
res

segm <- segmkf(n = n, K = 3, nrep = 1)
res <- gridcv(
    Xtrain, ytrain, 
    segm, score = err, 
    fun = plslda_agg, 
    pars = pars,
    verb = TRUE)
res

}

\keyword{datagen}