\name{euclsq}
\alias{euclsq}
\alias{euclsq_mu}
\alias{mahsq}
\alias{mahsq_mu}
\encoding{latin1}

\title{Matrix of distances}

\description{

----- Matrix (\eqn{n, m}) of distances between row observations of two datasets \eqn{X} (\eqn{n, p}) and Y (\eqn{m, p})

- \code{euclsq}: Squared Euclidean distance

- \code{mahsq}: Squared Mahalanobis distance

----- Matrix (\eqn{n, 1}) of distances between row observations of a dataset \eqn{X} (\eqn{n, p}) and a vector \eqn{p} (\eqn{n})

- \code{euclsq_mu}: Squared Euclidean distance

- \code{mahsq_mu}: Squared Euclidean distance

}

\usage{

euclsq(X, Y = NULL)

euclsq_mu(X, mu)

mahsq(X, Y = NULL, Uinv = NULL)

mahsq_mu(X, mu, Uinv = NULL)

}

\arguments{

\item{X}{X-data (\eqn{n, p}). }


\item{Y}{Data (\eqn{m, p}) compared to \code{X}. If \code{NULL} (default), \code{Y} is set equal to \code{X}.}

\item{mu}{Vector (\eqn{p}) compared to \code{X}.}

\item{Uinv}{For Mahalanobis distance. The inverse of a Choleski factorization matrix of the covariance matrix of \code{X}. If \code{NULL} (default), \code{Uinv} is calculated internally.} 

}

\value{A distance matrix.}

\examples{

n <- 5 ; p <- 3
X <- matrix(rnorm(n * p), ncol = p)

euclsq(X)
as.matrix(stats::dist(X)^2)
euclsq(X, X)

Y <- X[c(1, 3), ]
euclsq(X, Y)
euclsq_mu(X, Y[2, ])

i <- 3
euclsq(X, X[i, , drop = FALSE])
euclsq_mu(X, X[i, ])

S <- cov(X) * (n - 1) / n
i <- 3
mahsq(X)[i, , drop = FALSE]
stats::mahalanobis(X, X[i, ], S)

mahsq(X)
Y <- X[c(1, 3), ]
mahsq(X, Y)

}

\keyword{datagen}