\name{lwplsda}
\alias{lwplsda}
\alias{predict.Lwplsda}

\encoding{latin1}

\title{KNN-LWPLSDA}

\description{

Function \code{lwplsda} fits KNN-LWPLSDA models. This is the same methodology as for \code{\link{lwplsr}} except that PLSR is replaced by PLSDA. See the help page of \code{\link{lwplsr}} for details.

}

\usage{

lwplsda(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv
    )

\method{predict}{Lwplsda}(object, X, ..., nlv = NULL)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}).}

\item{nlvdis}{The number of LVs to consider in the global PLS used for the dimension reduction before calculating the dissimilarities. If \code{nlvdis = 0}, there is no dimension reduction.}

\item{diss}{The type of dissimilarity used for defining the neighbors. Possible values are "eucl" (default; Euclidean distance), "mahal" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

\item{h}{A scale scalar defining the shape of the weight function. Lower is \eqn{h}, sharper is the function. See \code{\link{wdist}}.}

\item{k}{The number of nearest neighbors to select for each observation to predict.}

\item{nlv}{The number(s) of LVs to calculate in the local PLSDA models.}

\item{object}{A fitted model, output of a call to the main function.}

\item{...}{Optional arguments.}

}

\value{

See the examples.

}

\examples{

n <- 50 ; p <- 7
X <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
y <- sample(c(1, 4, 10), size = n, replace = TRUE)
Xtrain <- X ; ytrain <- y
m <- 5
Xtest <- X[1:m, ] ; ytest <- y[1:m]

nlvdis <- 5 ; diss <- "mahal"
h <- 2 ; k <- 10
nlv <- 2  
fm <- lwplsda(
    Xtrain, ytrain, 
    nlvdis = nlvdis, diss = diss,
    h = h, k = k,
    nlv = nlv
    )
res <- predict(fm, Xtest)
res$pred
res$listnn

res <- predict(fm, Xtest, nlv = nlv)
res$pred

res <- predict(fm, Xtest, nlv = 0:2)
res$pred

}

