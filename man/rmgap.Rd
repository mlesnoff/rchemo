\name{rmgap}
\alias{rmgap}
\encoding{latin1}

\title{Removing vertical gaps in spectra}

\description{

Remove the vertical gaps in spectra (rows of matrix \eqn{X}), e.g. for ASD. This is done by extrapolation from simple linear regressions computed on the left side of the gaps.

}

\usage{rmgap(X, indexcol, k = 5)}

\arguments{

\item{X}{A dataset.}

\item{indexcol}{The column indexes corresponding to the gaps. For instance, if two gaps are observed between indexes 651-652 and between indexes 1451-1452, respectively, then \code{indexcol = c(651, 1451)}.}

\item{k}{The number of columns used on the left side of the gaps for fitting the linear regressions.}

}

\value{The corrected data \eqn{X}.}

\examples{

data(asdgap)
X <- asdgap$X

## Two gaps are at wavelengths 1000-1001 nm and 1800-1801 nm

indexcol <- which(colnames(X) == "1000" | colnames(X) == "1800")
indexcol
plotsp(X, lwd = 1.5)
abline(v = as.numeric(colnames(X)[1]) + indexcol - 1, col = "lightgrey", lty = 3)

zX <- rmgap(X, indexcol = indexcol)
plotsp(zX, lwd = 1.5)
abline(v = as.numeric(colnames(zX)[1]) + indexcol - 1, col = "lightgrey", lty = 3)

}

\keyword{datagen}