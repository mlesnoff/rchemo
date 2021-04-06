\name{sampks}
\alias{sampks}
\encoding{latin1}

\title{Kennard-Stone sampling}

\description{

The function divides the data \eqn{X} in two sets, "train" vs "test", using the Kennard-Stone (KS) algorithm (Kennard & Stone, 1969). The two sets correspond to two different underlying probability distributions: set "train" has higher dispersion than set "test".

}

\usage{

sampks(X, k, diss = c("eucl", "mahal"))

}

\arguments{

\item{X}{X-data (\eqn{n, p}) to be sampled.}

\item{k}{An integer defining the number of training observations to select.}

\item{diss}{The type of dissimilarity used for selecting the observations in the algorithm. Possible values are "eucl" (default; Euclidean distance) or "mahal" (Mahalanobis distance).}

}

\value{Indexes (i.e. row numbers in \eqn{X}) for sets "train" and "test".}

\references{

Kennard, R.W., Stone, L.A., 1969. Computer aided design of experiments. Technometrics, 11(1), 137-148.

}

\examples{

n <- 10 ; p <- 3
X <- matrix(rnorm(n * p), ncol = p)

k <- 7
sampks(X, k = k)  

n <- 10 ; k <- 25
X <- expand.grid(1:n, 1:n)
X <- X + rnorm(nrow(X) * ncol(X), 0, .1)
s <- sampks(X, k)$train 
plot(X)
points(X[s, ], pch = 19, col = 2, cex = 1.5)

}

\keyword{datagen}