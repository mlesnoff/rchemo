\name{transform}
\alias{transform}
\alias{coef}
\alias{predict}
\encoding{latin1}

\title{Generic functions}

\description{Generic auxiliary functions.}

\usage{

transform(fm, X, ...)

coef(fm, ...)

predict(fm, X, ...) 

}

\arguments{

\item{fm}{A fitted model, output of a call to a fitting function.}

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

fm <- plskern(X, Y, nlv = 3)
coef(fm)
coef(fm, nlv = 2)
transform(fm, X[1:2, ], nlv = 2)
predict(fm, X[1:2, ])
predict(fm, X[1:2, ], nlv = 0:3)

}

\keyword{datagen}