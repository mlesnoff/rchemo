\name{xfit}
\alias{xfit}
\alias{xfit.Pca}
\alias{xfit.Plsr}
\alias{xresid}
\encoding{latin1}

\title{Matrix fitting from a PCA or PLS model}

\description{

Function \code{xfit} calculates an approximate of matrix \eqn{X} (\eqn{X_fit}) from a PCA or PLS fitted on \eqn{X}.

Function \code{xresid} calculates the residual matrix \eqn{E = X - X_fit}.

}

\usage{

xfit(object, X, ...)

\method{xfit}{Pca}(object, X, ..., nlv = NULL) 

\method{xfit}{Plsr}(object, X, ..., nlv = NULL) 

xresid(object, X, ..., nlv = NULL)

}

\arguments{

\item{object}{A fitted model, output of a call to a fitting function.}

\item{X}{The X-data that was used to fit the model \code{object}.}

\item{nlv}{Number of components (PCs or LVs) to consider.}

\item{...}{Optional arguments.}

}

\value{See the examples.}

\examples{

n <- 6 ; p <- 4
X <- matrix(rnorm(n * p), ncol = p)
y <- rnorm(n)

nlv <- 3
fm <- pcasvd(X, nlv = nlv)
xfit(fm, X)
xfit(fm, X, nlv = 1)
xfit(fm, X, nlv = 0)

X - xfit(fm, X)
xresid(fm, X)

X - xfit(fm, X, nlv = 1)
xresid(fm, X, nlv = 1)

}

\keyword{datagen}