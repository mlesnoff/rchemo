\name{sampdp}
\alias{sampdp}
\encoding{latin1}

\title{Duplex sampling}

\description{

The function divides the data \eqn{X} in two sets, "train" vs "test", using the Duplex algorithm (Snee, 1977). The two sets are of equal size. If needed, the user can add \eqn{a posteriori} the eventual remaining observations (not in "train" nor "test") to "train".

}

\usage{

sampdp(X, k, diss = c("eucl", "mahal"))

}

\arguments{

\item{X}{X-data (\eqn{n, p}) to be sampled.}

\item{k}{An integer defining the number of training observations to select. Must be <= \eqn{n / 2}.}

\item{diss}{The type of dissimilarity used for selecting the observations in the algorithm. Possible values are "eucl" (default; Euclidean distance) or "mahal" (Mahalanobis distance).}

}

\value{Indexes (i.e. row numbers in \eqn{X}) for sets "train" and "test".}

\references{

Kennard, R.W., Stone, L.A., 1969. Computer aided design of experiments. Technometrics, 11(1), 137-148.

Snee, R.D., 1977. Validation of Regression Models: Methods and Examples. Technometrics 19, 415-428. https://doi.org/10.1080/00401706.1977.10489581


}

\examples{

n <- 10 ; p <- 3
X <- matrix(rnorm(n * p), ncol = p)

k <- 4
sampdp(X, k = k)
sampdp(X, k = k, diss = "mahal")

}

\keyword{datagen}