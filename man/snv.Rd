\name{snv}
\alias{snv}
\encoding{latin1}

\title{Standard normal variate transformation (SNV)}

\description{
SNV transformation of the row observations (e.g. spectra) of a dataset. By default, each observation is centered on its mean and divided by its standard deviation.
}

\usage{snv(X, center = TRUE, scale = TRUE)}

\arguments{

\item{X}{X-data (\eqn{n, p}).}

\item{center}{Logical. If \code{TRUE} (default), the centering in the SNV is done.}

\item{scale}{Logical. If \code{TRUE} (default), the scaling in the SNV is done.}

}

\value{A matrix of the transformed data.}

\examples{

data(cassav)

X <- cassav$Xtest

Xp <- snv(X)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(X, main = "Signal")
plotsp(Xp, main = "Corrected signal")
abline(h = 0, lty = 2, col = "grey")
par(oldpar)


}

\keyword{datagen}