\name{pinv}
\alias{pinv}
\encoding{latin1}

\title{Moore-Penrose pseudo-inverse of a matrix}

\description{

Calculation of the Moore-Penrose (MP) pseudo-inverse of a matrix \eqn{X}.

}

\usage{

pinv(X, tol = sqrt(.Machine$double.eps))

}

\arguments{

\item{X}{X-data (\eqn{n, p}).}

\item{tol}{A relative tolerance to detect zero singular values. }

}



\value{

A list of outputs, such as:

\item{Xplus}{The MP pseudo-inverse.}

}


\examples{

n <- 7 ; p <- 4
X <- matrix(rnorm(n * p), ncol = p)
y <- rnorm(n)

pinv(X)

tcrossprod(pinv(X)$Xplus, t(y))
lm(y ~ X - 1)

}

\keyword{datagen}