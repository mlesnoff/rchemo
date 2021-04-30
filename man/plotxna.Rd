\name{plotxna}
\alias{plotxna}
\encoding{latin1}

\title{Plotting Missing Data in a Matrix}

\description{Plot the location of missing data in a matrix.}

\usage{
plotxna(X, pch = 16, col = "red", grid = FALSE, asp = 0, ...)
}

\arguments{

\item{X}{A data set (\eqn{n x p}).}

\item{pch}{Type of point. See \code{\link{points}}.}

\item{col}{A color defining the color of the points.}

\item{grid}{Logical. If \code{TRUE}, a grid is plotted for representing the matrix rows an columns. Default to \code{FALSE}.}

\item{asp}{Scalar. Giving the aspect ratio y/x. The value \code{asp = 0} is the default in \code{\link{plot.default}} (no constraints on the ratio). See \code{\link{plot.default}}.}

\item{...}{Other arguments to pass in functions \code{\link{plot}}.}

}

\value{A plot.}

\examples{

data(octane)
X <- octane$X
n <- nrow(X)
p <- ncol(X)
N <- n * p

s <- sample(1:N, size = 50)
zX <- replace(X, s, NA)
plotxna(zX)
plotxna(zX, grid = TRUE, asp = 0)

}

\keyword{datagen}