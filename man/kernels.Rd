\name{krbf}
\alias{krbf}
\alias{kpol}
\alias{ktanh}
\encoding{latin1}

\title{Kernel functions}

\description{

Building Gram matrices for different kernels (e.g. Scholkopf & Smola 2002).

}

\usage{

krbf(X, Y = NULL, sigma = 1)

kpol(X, Y = NULL, degree = 1, scale = 1, offset = 0)

ktanh(X, Y = NULL, scale = 1, offset = 0)

}

\arguments{

\item{X}{Dataset (\eqn{n, p}).}

\item{Y}{Dataset (\eqn{m, p}). The resulting Gram matrix \eqn{K(X, Y)} has dimensionnality (\eqn{n, m}). If \code{NULL} (default), \code{Y} is set equal to \code{X}.}

\item{sigma}{The RBF kernel is defined by \eqn{exp(-.5 * (x - y)^2 / sigma^2)}.}

\item{degree}{See the code.}

\item{scale}{See the code.}

\item{offset}{See the code.}

}


\value{See the examples.}


\references{

Scholkopf, B., Smola, A.J., 2002. Learning with kernels: support vector machines, regularization, optimization, and beyond, Adaptive computation and machine learning. MIT Press, Cambridge, Mass.

}

\examples{

n <- 5 ; p <- 3
Xtrain <- matrix(rnorm(n * p), ncol = p)
Xtest <- Xtrain[1:2, , drop = FALSE] 

sigma <- .8
krbf(Xtrain, sigma = sigma)

krbf(Xtest, Xtrain, sigma = sigma)
exp(-.5 * euclsq(Xtest, Xtrain) / sigma^2)

kpol(Xtrain, degree = 2, scale = 1, offset = 1)

}

\keyword{datagen}