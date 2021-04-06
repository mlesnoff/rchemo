\name{detrend}
\alias{detrend}
\encoding{latin1}

\title{Polynomial de-trend transformation}

\description{

Polynomial de-trend transformation of row observations (e.g. spectra) of a dataset. The function fits an orthogonal polynom of a given degree to each observation and returns the residuals.

}

\usage{detrend(X, degree = 1)}

\arguments{

\item{X}{X-data (\eqn{n, p}).}

\item{degree}{Degree of the polynom.}

}

\details{\code{detrend} uses function \code{\link{poly}} of package \code{stats}.}

\value{A matrix of the transformed data.}

\examples{

data(cassav)

X <- cassav$Xtest

degree <- 1
Xp <- detrend(X, degree = degree)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(X, main = "Signal")
plotsp(Xp, main = "Corrected signal")
abline(h = 0, lty = 2, col = "grey")
par(oldpar)

}

\keyword{datagen}