\name{krbf}
\alias{krbf}
\alias{kpol}
\alias{ktanh}
\encoding{latin1}

\title{Kernel functions}

\description{

Building Gram matrices for different kernels (e.g. Scholkopf & Smola 2002).

- radial basis:    exp(-gamma * |x - y|^2)

- polynomial:      (gamma * x' * y + coef0)^degree

- sigmoid:        tanh(gamma * x' * y + coef0)

}

\usage{

krbf(X, Y = NULL, gamma = 1)

kpol(X, Y = NULL, degree = 1, gamma = 1, coef0 = 0)

ktanh(X, Y = NULL, gamma = 1, coef0 = 0)

}

\arguments{

\item{X}{Dataset (\eqn{n, p}).}

\item{Y}{Dataset (\eqn{m, p}). The resulting Gram matrix \eqn{K(X, Y)} has dimensionnality (\eqn{n, m}). If \code{NULL} (default), \code{Y} is set equal to \code{X}.}

\item{gamma}{See the code.}

\item{degree}{See the code.}

\item{coef0}{See the code.}

}


\value{See the examples.}


\references{

Scholkopf, B., Smola, A.J., 2002. Learning with kernels: support vector machines, regularization, optimization, and beyond, Adaptive computation and machine learning. MIT Press, Cambridge, Mass.

}

\examples{

n <- 5 ; p <- 3
Xtrain <- matrix(rnorm(n * p), ncol = p)
Xtest <- Xtrain[1:2, , drop = FALSE] 

gamma <- .8
krbf(Xtrain, gamma = gamma)

krbf(Xtest, Xtrain, gamma = gamma)
exp(-.5 * euclsq(Xtest, Xtrain) / gamma^2)

kpol(Xtrain, degree = 2, gamma = .5, coef0 = 1)

}

\keyword{datagen}