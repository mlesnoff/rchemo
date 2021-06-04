\name{plsr_agg}
\alias{plsr_agg}
\alias{predict.Plsr_agg}
\encoding{latin1}

\title{PLSR with aggregation of latent variables}

\description{

Ensemblist approach where the predictions are calculated by averaging the predictions of PLSR models (\code{\link{plskern}}) built with different numbers of latent variables (LVs). 

For instance, if argument \code{nlv} is set to \code{nlv = "5:10"}, the prediction for a new observation is the average (without weighting) of the predictions returned by the models with 5 LVS, 6 LVs, ... 10 LVs.

}

\usage{

plsr_agg(X, Y, weights = NULL, nlv)

\method{predict}{Plsr_agg}(object, X, ...)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{weights}{Weights (\eqn{n, 1}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{nlv}{A character string such as "5:20" defining the range of the numbers of LVs to consider (here: the models with nb LVS = 5, 6, ..., 20 are averaged). Syntax such as "10" is also allowed (here: correponds to the single model with 10 LVs).}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\examples{

n <- 20 ; p <- 4
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- rnorm(n)
Ytrain <- cbind(y1 = ytrain, y2 = 100 * ytrain)
m <- 3
Xtest <- Xtrain[1:m, , drop = FALSE] 
Ytest <- Ytrain[1:m, , drop = FALSE] ; ytest <- Ytest[1:m, 1]

nlv <- "1:3"
#nlv <- "2:3"
fm <- plsr_agg(Xtrain, ytrain, nlv = nlv)
names(fm)

## Maximal PLSR model
zfm <- fm$fm
class(zfm)
names(zfm)
summary(zfm, Xtrain)

##### Predictions
res <- predict(fm, Xtest)
names(res)
## Final predictions (after aggregation)
res$pred
msep(res$pred, ytest)
## Intermediate predictions (Per nb. LVs)
res$predlv

## Gridscore
## Here, there is no sense to use gridscorelv 
pars <- mpars(nlv = c("1:3", "2:5"))
## Same as:
## pars <- list(nlv = c("1:3", "2:5"))
pars
res <- gridscore(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = plsr_agg, 
    pars = pars)
res

## Gridcv
## Here, there is no sense to use gridcvlv 
K = 3
segm <- segmkf(n = n, K = K, nrep = 1)
segm
res <- gridcv(
    Xtrain, Ytrain, 
    segm, score = msep, 
    fun = plsr_agg, 
    pars = pars,
    verb = TRUE)
res

}

\keyword{datagen}