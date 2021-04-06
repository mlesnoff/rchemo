\name{mavg}
\alias{mavg}
\encoding{latin1}

\title{Smoothing by moving average}

\description{Smoothing, by moving average, of the row observations (e.g. spectra) of a dataset.}

\usage{mavg(X, n = 5)}

\arguments{

\item{X}{X-data (\eqn{n, p}).}

\item{n}{The number of points (i.e. columns of \code{X}) defining the window over wich is calculate each average. The smoothing is calculated for the point at the center of the window. Therefore, \code{n} must be an odd integer, and be higher or equal to 3.}

}

\value{A matrix of the transformed data.}

\examples{

data(cassav)

X <- cassav$Xtest
headm(X)

Xp <- mavg(X, n = 11)
headm(Xp)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(X, main = "Signal")
plotsp(Xp, main = "Corrected signal")
abline(h = 0, lty = 2, col = "grey")
par(oldpar)

}

\keyword{datagen}