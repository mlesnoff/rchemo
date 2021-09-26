\name{lwplsr_agg}
\alias{lwplsr_agg}
\alias{predict.Lwplsr_agg}

\encoding{latin1}

\title{Aggregation of KNN-LWPLSR models with different numbers of LVs}

\description{

Ensemblist method where the predictions are calculated by averaging the predictions of KNN-LWPLSR models (\code{\link{lwplsr}}) built with different numbers of latent variables (LVs). 

For instance, if argument \code{nlv} is set to \code{nlv = "5:10"}, the prediction for a new observation is the simple average of the predictions returned by the models with 5 LVS, 6 LVs, ... 10 LVs, respectively. 

}

\usage{

lwplsr_agg(
    X, Y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    cri = 4,
    verb = FALSE
    )

\method{predict}{Lwplsr_agg}(object, X, ...)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{nlvdis}{The number of LVs to consider in the global PLS used for the dimension reduction before calculating the dissimilarities. If \code{nlvdis = 0}, there is no dimension reduction.}

\item{diss}{The type of dissimilarity used for defining the neighbors. Possible values are "eucl" (default; Euclidean distance), "mahal" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

\item{h}{A scale scalar defining the shape of the weight function. Lower is \eqn{h}, sharper is the function. See \code{\link{wdist}}.}

\item{k}{The number of nearest neighbors to select for each observation to predict.}

\item{nlv}{A character string such as "5:20" defining the range of the numbers of LVs to consider (here: the models with nb LVS = 5, 6, ..., 20 are averaged). Syntax such as "10" is also allowed (here: correponds to the single model with 10 LVs).}

\item{cri}{Argument \code{cri} in function \code{\link{wdist}}.}

\item{verb}{Logical. If \code{TRUE}, fitting information are printed.}

\item{object}{A fitted model, output of a call to the main function.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\examples{

n <- 30 ; p <- 10
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- rnorm(n)
Ytrain <- cbind(ytrain, 100 * ytrain)
m <- 4
Xtest <- matrix(rnorm(m * p), ncol = p)
ytest <- rnorm(m)
Ytest <- cbind(ytest, 10 * ytest)

nlvdis <- 5 ; diss <- "mahal"
h <- 2 ; k <- 10
nlv <- "2:6" 
fm <- lwplsr_agg(
    Xtrain, Ytrain, 
    nlvdis = nlvdis, diss = diss,
    h = h, k = k,
    nlv = nlv)
names(fm)
res <- predict(fm, Xtest)
names(res)
res$pred
msep(res$pred, Ytest)

## Gridscore
## Here, there is no sense to use gridscorelv 
nlvdis <- 5 ; diss <- "mahal"
h <- c(2, Inf)
k <- c(10, 20)
nlv <- c("1:3", "2:5")
pars <- mpars(nlvdis = nlvdis, diss = diss,
    h = h, k = k, nlv = nlv)
pars
res <- gridscore(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = lwplsr_agg, 
    pars = pars)
res

## Gridcv
## Here, there is no sense to use gridcvlv 
K = 3
segm <- segmkf(n = n, K = K, nrep = 1)
segm
res <- gridcv(
    Xtrain, Ytrain, 
    segm, score = msep, 
    fun = lwplsr_agg, 
    pars = pars,
    verb = TRUE)
res

}

