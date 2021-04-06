\name{dderiv}
\alias{dderiv}
\encoding{latin1}

\title{Derivation by finite difference}

\description{
Calculation of the first derivatives, by finite differences, of the row observations (e.g. spectra) of a dataset. 
}

\usage{dderiv(X, n = 5, ts = 1)}

\arguments{

\item{X}{X-data (\eqn{n, p}).}

\item{n}{The number of points (i.e. columns of \code{X}) defining the window over wich is calculate each finite difference. The derivation is calculated for the point at the center of the window. Therefore, \code{n} must be an odd integer, and be higher or equal to 3.}

\item{ts}{A scaling factor for the finite differences (by default, \code{ts} = 1.)}
}

\value{A matrix of the transformed data.}


\examples{

data(cassav)

X <- cassav$Xtest

n <- 15
Xp <- dderiv(X, n = n)            # = 1st derivate
#Xp <- dderiv(dderiv(X, n), n)    # = 2nd derivate

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
plotsp(X, main = "Signal")
plotsp(Xp, main = "Corrected signal")
abline(h = 0, lty = 2, col = "grey")
par(oldpar)

}

\keyword{datagen}