\name{getknn}
\alias{getknn}
\encoding{latin1}

\title{KNN selection}

\description{

Function \code{getknn} selects the \eqn{k} nearest neighbours of each row observation of a new data set (= query) within a training data set, based on a dissimilarity measure. 

\code{getknn} uses function \code{\link{get.knnx}} of package \code{FNN} (Beygelzimer et al.) available on CRAN.  

}

\usage{

getknn(Xtrain, X, k = NULL, diss = c("eucl", "mahal"), 
  algorithm = "brute", list = TRUE)

}

\arguments{

\item{Xtrain}{Training X-data (\eqn{n, p}).}

\item{X}{New X-data (\eqn{m, p}) to consider.}

\item{k}{The number of nearest neighbors to select in \code{Xtrain} for each observation of \code{X}.}

\item{diss}{The type of dissimilarity used. Possible values are "eucl" (default; Euclidean distance) or "mahal" (Mahalanobis distance).}

\item{algorithm}{Search algorithm used for Euclidean and Mahalanobis distances. Default to \code{"brute"}. See \code{\link{get.knnx}}.}

\item{list}{If \code{TRUE} (default), a list format is also returned for the outputs.}

}

\value{

A list of outputs, such as:

\item{nn}{A dataframe (\eqn{m x k}) with the indexes of the neighbors.}

\item{d}{A dataframe (\eqn{m x k}) with the dissimilarities between the neighbors and the new observations.
}

\item{listnn}{Same as \code{$nn} but in a list format.}

\item{listd}{Same as \code{$d} but in a list format.}

}

\examples{

n <- 10
p <- 4
X <- matrix(rnorm(n * p), ncol = p)
Xtrain <- X
Xtest <- X[c(1, 3), ]
m <- nrow(Xtest)

k <- 3
getknn(Xtrain, Xtest, k = k)

fm <- pcasvd(Xtrain, nlv = 2)
Ttrain <- fm$T
Ttest <- transform(fm, Xtest)
getknn(Ttrain, Ttest, k = k, diss = "mahal")

}

\keyword{datagen}