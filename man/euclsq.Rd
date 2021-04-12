\name{euclsq}
\alias{euclsq}
\alias{mahsq}
\alias{euclsq_mu}
\alias{mahsq_mu}
\encoding{latin1}

\title{Matrix of distances}

\description{

Matrix (\eqn{n, m}) of distances between row observations of two datasets \eqn{X} (\eqn{n, p}) and Q (\eqn{m, p})

- \code{euclsq}: Squared Euclidean distance

- \code{mahsq}: Squared Mahalanobis distance

Matrix (\eqn{n, 1}) of distances between row observations of a dataset \eqn{X} (\eqn{n, p}) and a vector \eqn{p} (\eqn{n})

- \code{euclsq_mu}: Squared Euclidean distance

- \code{mahsq_mu}: Squared Euclidean distance

}

\usage{

euclsq(X, Q = NULL)

mahsq(X, Q = NULL, U = NULL)

euclsq_mu(X, mu)

mahsq_mu(X, mu, U = NULL)

}

\arguments{

\item{X}{X-data (\eqn{n, p}). }


\item{Q}{Data (\eqn{m, p}) compared to \code{X}. If \code{NULL} (default), \code{Q} is set equal to \code{X}.}

\item{mu}{Vector (\eqn{p}) compared to \code{X}.}

\item{U}{For Mahalanobis distance. A Choleski factorization matrix of the covariance matrix of \code{X}. If \code{NULL} (default), \code{U} is calculated internally.} 

}

\value{A distance matrix.}

\examples{

n <- 5 ; p <- 3
X <- matrix(rnorm(n * p), ncol = p)
qr(X)$rank

euclsq(X)
as.matrix(stats::dist(X)^2)
euclsq(X, X)

Q <- X[c(1, 3), ]
euclsq(X, Q)
euclsq_mu(X, Q[2, ])

i <- 3
euclsq(X, X[i, , drop = FALSE])
euclsq_mu(X, X[i, ])


S <- cov(X) * (n - 1) / n
i <- 3
mahsq(X)[i, , drop = FALSE]
stats::mahalanobis(X, X[i, ], S)

mahsq(X)
Q <- X[c(1, 3), ]
mahsq(X, Q)

}

\keyword{datagen}