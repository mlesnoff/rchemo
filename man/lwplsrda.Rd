\name{lwplsrda}
\alias{lwplsrda}
\alias{lwplslda}
\alias{lwplsqda}
\alias{predict.Lwplsrda}
\alias{predict.Lwplsprobda}
\encoding{latin1}

\title{KNN-LWPLS-DA Models}

\description{

- \code{lwplsrda}: KNN-LWPLSRDA models. This is the same methodology as for \code{\link{lwplsr}} except that PLSR is replaced by PLSRDA (\code{\link{plsrda}}). See the help page of \code{\link{lwplsr}} for details.

- \code{lwplslda} and \code{lwplsqda}: Same as above, but PLSRDA is replaced by either PLSLDA (\code{\link{plslda}}) or PLSQDA ((\code{\link{plsqda}}), respecively.

}

\usage{

lwplsrda(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    cri = 4,
    verb = FALSE
    )

lwplslda(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    prior = c("unif", "prop"),
    cri = 4,
    verb = FALSE
    ) 

lwplsqda(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    prior = c("unif", "prop"),
    cri = 4,
    verb = FALSE
    ) 

\method{predict}{Lwplsrda}(object, X, ..., nlv = NULL)  

\method{predict}{Lwplsprobda}(object, X, ..., nlv = NULL)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}). \bold{Note:} If \code{y} is a factor, it is replaced by a character vector.}

\item{nlvdis}{The number of LVs to consider in the global PLS used for the dimension reduction before calculating the dissimilarities. If \code{nlvdis = 0}, there is no dimension reduction.}

\item{diss}{The type of dissimilarity used for defining the neighbors. Possible values are "eucl" (default; Euclidean distance), "mahal" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

\item{h}{A scale scalar defining the shape of the weight function. Lower is \eqn{h}, sharper is the function. See \code{\link{wdist}}.}

\item{k}{The number of nearest neighbors to select for each observation to predict.}

\item{nlv}{The number(s) of LVs to calculate in the local PLSDA models.}

\item{prior}{The prior probabilities of the classes. Possible values are "unif" (default; probabilities are set equal for all the classes) or "prop" (probabilities are set equal to the observed proportions of the classes in \code{y}).}

\item{cri}{Argument \code{cri} in function \code{\link{wdist}}.}

\item{verb}{Logical. If \code{TRUE}, fitting information are printed.}

\item{object}{A fitted model, output of a call to the main function.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\examples{

n <- 50 ; p <- 7
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- sample(c(1, 4, 10), size = n, replace = TRUE)
m <- 4
Xtest <- matrix(rnorm(m * p), ncol = p)
ytest <- sample(c(1, 4, 10), size = m, replace = TRUE)

nlvdis <- 5 ; diss <- "mahal"
h <- 2 ; k <- 10
nlv <- 2  
fm <- lwplsrda(
    Xtrain, ytrain, 
    nlvdis = nlvdis, diss = diss,
    h = h, k = k,
    nlv = nlv
    )
res <- predict(fm, Xtest)
res$pred
res$listnn
err(res$pred, ytest)

res <- predict(fm, Xtest, nlv = 0:2)
res$pred


}

