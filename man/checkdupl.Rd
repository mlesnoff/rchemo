\name{checkdupl}
\alias{checkdupl}
\alias{rmdupl}
\encoding{latin1}

\title{Find or remove duplicated row observations between two datasets}

\description{

- \code{checkdupl}: Finds the duplicated row observations between two datasets.

- \code{rmdupl}: Removes the duplicated row observations between two datasets.

}

\usage{

checkdupl(X, Y, nam = NULL, digits = NULL, check.all = FALSE)

rmdupl(X, nam = NULL, digits = NULL, check.all = FALSE)

}

\arguments{

\item{X}{A datset compared to \code{Y}.}

\item{Y}{A dataset compared to \code{X}.}

\item{nam}{Names of the variables to consider in \code{X} and \code{Y} in the duplication test. If \code{NULL} (default), \code{nam} is set to all the column names of \code{X}. Variables defined in \code{nam} must be common between \code{X} and \code{Y}.}

\item{digits}{The number of digits used when rounding the variables (in \code{nam}) before the duplication test. Default to \code{NULL} (no rounding.}

\item{check.all}{Logical (default = \code{FALSE}). If \code{TRUE}, an additionnal test of duplication is done considering all the columns of \code{X} (even if \code{nam} is defined as a part of these columns).}

}

\value{See the examples.}

\examples{

X1 <- matrix(c(1:5, 1:5, c(1, 2, 7, 4, 8)), nrow = 3, byrow = TRUE)
dimnames(X1) <- list(1:3, c("v1", "v2", "v3", "v4", "v5"))

X2 <- matrix(c(6:10, 1:5, c(1, 2, 7, 6, 12)), nrow = 3, byrow = TRUE)
dimnames(X2) <- list(1:3, c("v1", "v2", "v3", "v4", "v5"))

X1
X2

checkdupl(X1, X2)

checkdupl(X1, X2, nam = c("v1", "v2"))

checkdupl(X1, X2, nam = c("v1", "v2"), check.all = TRUE)

z <- checkdupl(X1, X1)
z[z$rownum.X != z$rownum.Y, ]

z <- checkdupl(X1, X1, nam = c("v1", "v2"))
z[z$rownum.X != z$rownum.Y, ]

rmdupl(X1)

rmdupl(X1, nam = c("v1", "v2"))

}

\keyword{datagen}