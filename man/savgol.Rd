\name{savgol}
\alias{savgol}
\encoding{latin1}

\title{Savitzky-Golay smoothing}

\description{

Smoothing by derivation, with a Savitzky-Golay filter, of the row observations (e.g. spectra) of a data set .

The function uses function \code{\link{sgolayfilt}} of package \code{signal} available on the CRAN.

}

\usage{savgol(X, m, n, p, ts = 1)}

\arguments{

\item{X}{X-data).}

\item{m}{Derivation order.}

\item{n}{Filter length (must be odd), i.e. the number of colums in \code{X} defining the filter window.}

\item{p}{Polynomial order.}

\item{ts}{Scaling factor (e.g. the absolute step between two columns in matrix \eqn{X}), see argument \code{ts} in function \code{\link{sgolayfilt}}. This has not impact on the form of the transformed output.}

}

\value{A matrix of the transformed data.}

\examples{

X <- cassav$Xtest

m <- 1 ; n <- 11 ; p <- 2
Xp <- savgol(X, m, n, p)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(X, main = "Signal")
plotsp(Xp, main = "Corrected signal")
abline(h = 0, lty = 2, col = "grey")
par(oldpar)

}

\keyword{datagen}