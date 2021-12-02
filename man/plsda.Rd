\name{plsrda}
\alias{plsrda}
\alias{plslda}
\alias{plsqda}
\alias{predict.Plsrda}
\alias{predict.Plsprobda}
\encoding{latin1}

\title{PLSDA models}

\description{

Discrimination (DA) based on PLS.

The training variable \eqn{y} (univariate class membership) is firstly transformed to a dummy table containing \eqn{nclas} columns, where \eqn{nclas} is the number of classes present in \eqn{y}. Each column is a dummy variable (0/1). Then, a PLS2 is implemented on the \eqn{X-}data and the dummy table, returning latent variables (LVs) that are used as dependent variables in a DA model.

- \code{plsrda}: Usual "PLSDA". A linear regression model predicts the Y-dummy table from the PLS2 LVs. This corresponds to the PLSR2 of the X-data and the Y-dummy table. For a given observation, the final prediction is the class corresponding to the dummy variable for which the prediction is the highest.

- \code{plslda} and \code{plsqda}: Probabilistic LDA and QDA are run over the PLS2 LVs, respectively.

}

\usage{

plsrda(X, y, weights = NULL, nlv)

plslda(X, y, weights = NULL, nlv, prior = c("unif", "prop"))

plsqda(X, y, weights = NULL, nlv, prior = c("unif", "prop"))

\method{predict}{Plsrda}(object, X, ..., nlv = NULL) 

\method{predict}{Plsprobda}(object, X, ..., nlv = NULL) 

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}). \bold{Note:} If \code{y} is a factor, it is replaced by a character vector.}

\item{weights}{Weights (\eqn{n}) to apply to the training observations for the PLS2. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{nlv}{The number(s) of LVs to calculate.}

\item{prior}{The prior probabilities of the classes. Possible values are "unif" (default; probabilities are set equal for all the classes) or "prop" (probabilities are set equal to the observed proportions of the classes in \code{y}).}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\examples{

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- sample(c(1, 4, 10), size = n, replace = TRUE)
#ytrain <- sample(c("a", "10", "d"), size = n, replace = TRUE)
Xtest <- Xtrain[1:5, ] ; ytest <- ytrain[1:5]

#### PLSDA

nlv <- 5
fm <- plsrda(Xtrain, ytrain, nlv = nlv)
names(fm)
## The PLS2 model
zfm <- fm$fm
transform(zfm, Xtest)
transform(zfm, Xtest, nlv = 1)
summary(zfm, Xtrain)
coef(zfm)
coef(zfm, nlv = 0)
coef(zfm, nlv = 2)
## PLSDA
predict(fm, Xtest)
predict(fm, Xtest, nlv = 0:2)$pred

pred <- predict(fm, Xtest)$pred
err(pred, ytest)

#### PLS-LDA

nlv <- 5
fm <- plslda(Xtrain, ytrain, nlv = nlv)
predict(fm, Xtest)
predict(fm, Xtest, nlv = 1:2)$pred

## The PLS2 model
zfm <- fm[[1]][[1]]
class(zfm)
names(zfm)
summary(zfm, Xtrain)
transform(zfm, Xtest[1:2, ])
coef(zfm)

}

\keyword{datagen}