\name{gridscore}
\alias{gridscore}
\alias{gridscore_lv}

\encoding{latin1}

\title{Tuning predictive models}

\description{

Wrappers for tuning predictive models.

The functions return prediction scores (in general error rates) for a given model, a grid of parameter values and a validation dataset. 
 
Function \code{gridscore} can be used for any model. Function \code{gridscore_lv} is specific to models based on latent variables (e.g. PLSR) for which it is much faster than \code{gridscore} and recommended.

The grid (argument \code{pars}) must be a dataframe whose the column names correspond to the model parameters involved in the score calculations. For \code{gridscore_lv}, the grid must contain a column \code{nlv} (number of latent variables; LVs).   

}

\usage{

gridscore(Xtrain, Ytrain, X, Y, score, fun, pars, verb = FALSE)

gridscore_lv(Xtrain, Ytrain, X, Y, score, fun, pars, verb = FALSE)

}

\arguments{

\item{Xtrain}{Training X-data (\eqn{n, p}).}

\item{Ytrain}{Training Y-data (\eqn{n, q}).}

\item{X}{Validation X-data (\eqn{n, p}).}

\item{Y}{Validation Y-data (\eqn{n, q}).}

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
Xtrain <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
ytrain <- rnorm(n)
Ytrain <- cbind(ytrain, 10 * rnorm(n))
m <- 5
Xtest <- Xtrain[1:m, ] 
Ytest <- Ytrain[1:m, ] ; ytest <- Ytest[, 1]

nlv <- 5
pars <- data.frame(nlv = 1:nlv)
pars
gridscore(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, fun = plskern, 
    pars
    )
gridscore_lv(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, fun = plskern, 
    pars
    )

fm <- plskern(Xtrain, Ytrain, nlv = nlv)
pred <- predict(fm, Xtest)$pred
msep(pred, Ytest)

############## EXAMPLES WITH PLSLDA

n <- 50 ; p <- 30
X <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
y <- sample(c(1, 4, 10), size = n, replace = TRUE)
Xtrain <- X ; ytrain <- y
m <- 5
Xtest <- X[1:m, ] ; ytest <- y[1:m]

nlv <- 5
pars <- data.frame(nlv = 1:nlv)
gridscore(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, fun = plslda, 
    pars
    )
gridscore_lv(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, fun = plslda, 
    pars
    )
fm <- plslda(Xtrain, ytrain, nlv = nlv)
pred <- predict(fm, Xtest)$pred
err(pred, ytest)

pars <- expand.grid(
    nlv = 1:nlv,
    prior = c("unif", "prop"),
    stringsAsFactors = FALSE
    )
pars
gridscore(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, fun = plslda,
    pars
    )
gridscore_lv(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, fun = plslda,
    pars
    )


}

