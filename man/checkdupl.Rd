\name{checkdupl}
\alias{checkdupl}
\encoding{latin1}

\title{Find and remove duplicated rows between two datasets}

\description{

Finds the duplicated row observations between two datasets.

}

\usage{

checkdupl(X, Y = NULL, digits = NULL)

}

\arguments{

\item{X}{A dataset compared to \code{Y}.}

\item{Y}{A dataset compared to \code{X}.}

\item{digits}{The number of digits used when rounding the data before the duplication test. Default to \code{NULL} (no rounding.}

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

checkdupl(X1)

checkdupl(matrix(rnorm(20), nrow = 5))

## Removing duplicates

res <- checkdupl(X1)
s <- unique(res$rownum2)
zX1 <- X1[-s, ]
zX1

}

\keyword{datagen}