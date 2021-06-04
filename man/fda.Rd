\name{fda}
\alias{fda}
\alias{fdasvd}
\alias{transform.Fda}
\alias{summary.Fda}
\encoding{latin1}

\title{Factorial discriminant analysis}

\description{
Factorial discriminant analysis (FDA). The functions maximize the compromise \eqn{p'Bp / p'Wp}, i.e. \eqn{max p'Bp} with constraint \eqn{p'Wp = 1}. Vectors \eqn{p} are the linear discrimant coefficients "LD".

- \code{fda}: Eigen factorization of \eqn{W^(-1)B}

- \code{fdasvd}: Weighted SVD factorization of the matrix of the class centers.

If \eqn{W} is singular, W^(-1) is replaced by a MP pseudo-inverse.
}

\usage{

fda(X, y, nlv = NULL)

fdasvd(X, y, nlv = NULL)

\method{transform}{Fda}(object, X, ..., nlv = NULL) 

\method{summary}{Fda}(object, ...) 

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}). \bold{Note:} If \code{y} is a factor, it is replaced by a character vector.}

\item{nlv}{The number(s) of LVs to calculate.}

\item{object}{A fitted model, output of a call to the main function.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\references{

Saporta G., 2011. Probabilités analyse des données et statistique. Editions Technip, Paris, France.

}

\examples{

data(iris)

X <- iris[, 1:4]
y <- iris[, 5]
table(y)

fm <- fda(X, y)
headm(fm$T)
transform(fm, X[1:3, ])
## Tcenters = projection of the class centers in the score space
fm$Tcenters
## X-loadings matrix
## = coefficients of the linear discriminant function
## = "LD" of function lda of package MASS
fm$P
## Explained variance by PCA of the class centers 
## in transformed scale
summary(fm)
plotxy(fm$T, group = y, ellipse = TRUE, 
    zeroes = TRUE, pch = 16, cex = 1.5, ncol = 2)
points(fm$Tcenters, pch = 8, col = "blue", cex = 1.5)

}

\keyword{datagen}