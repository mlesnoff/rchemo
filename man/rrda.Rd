\name{rrda}
\alias{rrda}
\alias{predict.Rrda}
\encoding{latin1}

\title{RR-DA models}

\description{
Discrimination (DA) based on ridge regression (RR).

The training variable \eqn{y} (univariate class membership) is transformed to a dummy table containing \eqn{nclas} columns, where \eqn{nclas} is the number of classes present in \eqn{y}. Each column is a dummy variable (0/1). Then, a ridge regression (RR) is run on the \eqn{X-}data and the dummy table, returning predictions of the dummy variables. For a given observation, the final prediction is the class corresponding to the dummy variable for which the prediction is the highest.
}

\usage{

rrda(X, y, weights = NULL, lb = 1e-5)

\method{predict}{Rrda}(object, X, ..., lb = NULL) 

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}). \bold{Note:} If \code{y} is a factor, it is replaced by a character vector.}

\item{weights}{Weights (\eqn{n}) to apply to the training observations for the PLS2. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{lb}{A value of regularization parameter \eqn{lambda}. If \code{lb = 0}, a pseudo-inverse is used in the RR.}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\examples{

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- sample(c(1, 4, 10), size = n, replace = TRUE)
#ytrain <- sample(c("a", "10", "d"), size = n, replace = TRUE)
m <- 5
Xtest <- Xtrain[1:m, ] ; ytest <- ytrain[1:m]

lb <- 1
fm <- rrda(Xtrain, ytrain, lb = lb)
predict(fm, Xtest)

pred <- predict(fm, Xtest)$pred
err(pred, ytest)

predict(fm, Xtest, lb = 0:2)
predict(fm, Xtest, lb = 0)

}

\keyword{datagen}