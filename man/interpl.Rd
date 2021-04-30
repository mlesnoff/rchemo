\name{interpl}
\alias{interpl}
\encoding{latin1}

\title{Resampling of spectra by interpolation methods}

\description{

Resampling of signals by interpolation methods, including linear, spline, and cubic interpolation. The function uses \code{\link{interp1}} of package \code{signal} available on the CRAN.

}

\usage{

interpl(X, w, meth = "cubic", ...)

}

\arguments{

\item{X}{X-data (\eqn{n x p}). For the interpolation, the column names of \code{X} are taken as numeric values, \eqn{w0}. If they are not numeric or missing, they are automatically set to \code{w0 = 1:p}.}

\item{w}{A vector of the values where to interpolate (typically within the range of \eqn{w0}).}

\item{meth}{The method of interpolation. See \code{\link{interp1}}.}

\item{...}{Optional arguments to pass in function \code{\link{splinefun}} if \code{meth = "spline"}.}

}

\value{
A matrix of the interpolated signals.
}


\examples{

data(cassav)

X <- cassav$Xtest
headm(X)

w <- seq(500, 2400, length = 10)
zX <- interpl(X, w, meth = "spline")
headm(zX)
plotsp(zX)

}

\keyword{datagen}