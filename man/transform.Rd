\name{transform}
\alias{transform}
\encoding{latin1}

\title{Generic functions}

\description{Generic auxiliary functions.}

\usage{

transform(object, X, ...)

}

\arguments{

\item{object}{A fitted model, output of a call to a fitting function.}

\item{X}{New X-data (\eqn{m, p}) to consider.}

\item{...}{Optional arguments.}

}

\value{See the examples.}

\examples{

n <- 8 ; p <- 6
X <- matrix(rnorm(n * p), ncol = p)
y1 <- 100 * rnorm(n)
y2 <- 100 * rnorm(n)
Y <- cbind(y1, y2)

object <- plskern(X, Y, nlv = 3)
coef(object)
coef(object, nlv = 2)
transform(object, X[1:2, ], nlv = 2)
predict(object, X[1:2, ])
predict(object, X[1:2, ], nlv = 0:3)

}

\keyword{datagen}