\name{plsda}
\alias{plsda}
\alias{plslda}
\alias{plsqda}
\alias{predict.Plsda}
\alias{predict.Plsdaprob}
\encoding{latin1}

\title{PLSDA models}

\description{

Discrimination (DA) based on PLS latent variables (LVs).

The training class membership variable \eqn{y} (univariatel) is firstly transformed (with function \code{\link{dummy}}) to a dummy table containing \eqn{nclas} columns, where \eqn{nclas} is the number of classes present in \eqn{y}. Each column is a dummy variable (0/1). Then, a PLS2 is implemented on the \eqn{X-}data and the dummy table, returning LVs that are used as dependent variables in a DA model.

- \code{plsda}: Usual "PLDA": the final prediction (a given class) corresponds to the dummy variable for which the prediction (unbounded probability estimate) is the highest.

- \code{plslda} and \code{plsqda}: PLS-LDA and PLS-QDA, respectively. \code{\link{lda}} and \code{\link{qda}} are run over the PLS2 LVs.

}

\usage{

plsda(X, y, nlv, weights = NULL)

plslda(X, y, nlv, weights = NULL, prior = c("unif", "prop"))

plsqda(X, y, nlv, weights = NULL, prior = c("unif", "prop"))

\method{predict}{Plsda}(object, X, ..., nlv = NULL) 

\method{predict}{Plsdaprob}(object, X, ..., nlv = NULL) 

}

\arguments{

\item{X}{For the main functions and auxiliary function \code{summary}: Training X-data (\eqn{n, p}). --- For the other auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}).}

\item{nlv}{The number(s) of LVs to calculate.}

\item{weights}{Weights (\eqn{n}) to apply to the training observations for the PLS2. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{prior}{The prior probabilities of the classes. Possible values are "unif" (default; probabilities are set equal for all the classes) or "prop" (probabilities are set equal to the observed proportions of the classes in \code{y}).}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments.}

}

\value{

See the examples.

}

\examples{

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- sample(c(1, 4, 10), size = n, replace = TRUE)
#ytrain <- sample(c("a", "10", "d"), size = n, replace = TRUE)
Xtest <- Xtrain[1:5, ] ; ytest <- ytrain[1:5]

nlv <- 5
fm <- plsda(Xtrain, ytrain, nlv = nlv)
names(fm)
transform(fm, Xtest)
transform(fm, Xtest, nlv = 1)
summary(fm)
coef(fm)
coef(fm, nlv = 0)
coef(fm, nlv = 2)
predict(fm, Xtest)
predict(fm, Xtest, nlv = 0:2)$pred

#### PLS-LDA

nlv <- 5
fm <- plslda(Xtrain, ytrain, nlv = nlv)
predict(fm, Xtest)
predict(fm, Xtest, nlv = 1:2)$pred

## The PLS2 model
zfm <- fm[[1]]
class(zfm)
names(zfm)
summary(zfm)
transform(zfm, Xtest[1:2, ])
coef(zfm)

}

\keyword{datagen}