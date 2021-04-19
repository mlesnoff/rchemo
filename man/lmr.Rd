\name{lmr}
\alias{lmr}
\alias{coef.Lm}
\alias{predict.Lmr}
\encoding{latin1}

\title{Linear regression models}

\description{

Linear regression models (wrapper using function \code{\link{lm}}).

}

\usage{

lmr(X, Y, weights = NULL)

\method{coef}{Lm}(object, ...) 

\method{predict}{Lmr}(object, X, ...)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{weights}{Weights (\eqn{n, 1}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments.}

}

\value{See the examples.}

\examples{

n <- 8 ; p <- 3
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y <- rnorm(n)
Y <- cbind(y, rnorm(n))
Xtrain <- X[1:6, ] ; Ytrain <- Y[1:6, ]
Xtest <- X[7:8, ] ; Ytest <- Y[7:8, ]

fm <- lmr(Xtrain, Ytrain)
coef(fm)

predict(fm, Xtest)

pred <- predict(fm, Xtest)$pred
msep(pred, Ytest)

}

\keyword{datagen}