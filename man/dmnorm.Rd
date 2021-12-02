\name{dmnorm}
\alias{dmnorm}
\alias{predict.Dmnorm}
\encoding{latin1}

\title{Multivariate normal probability density}

\description{

Prediction of the normal probability density of multivariate observations.

}

\usage{

dmnorm(X = NULL, mu = NULL, sigma = NULL)

\method{predict}{Dmnorm}(object, X, ...)
  
}

\arguments{

\item{X}{For \code{dmnorm}: Training data (\eqn{n, p}) used for estimating the mean and the covariance matrixhe population (if \code{mu} or/and \code{sigma} are not provided). For \code{predict}: New data (\eqn{m, p}) for which the density has to be predicted.}

\item{mu}{The mean (\eqn{p, 1}) of the normal distribution. If \code{NULL} (default), \eqn{mu} is estimated by the column-wise mean of the training data.}

\item{sigma}{The covariance matrix (\eqn{p x p}) of the normal distribution. If \code{NULL} (default), \eqn{sigma} is estimated by the empirical covariance matrix (denominator \eqn{n - 1}) of the training data.}

\item{object}{A result of a call to \code{dmnorm}.}

\item{...}{Optional arguments.}

}

\value{

See the examples.

}


\examples{

data(iris)

X <- iris[, 1:2]

## Distribution of the class "Setosa"

Xtrain <- X[1:40, ]
Xtest <- X[40:50, ]

fm <- dmnorm(Xtrain)
fm

k <- 50
x1 <- seq(min(Xtrain[, 1]), max(Xtrain[, 1]), length.out = k)
x2 <- seq(min(Xtrain[, 2]), max(Xtrain[, 2]), length.out = k)
zX <- expand.grid(x1, x2)
pred <- predict(fm, zX)$pred
contour(x1, x2, matrix(pred, nrow = 50))

points(Xtest, col = "red", pch = 16)

}

\keyword{datagen}