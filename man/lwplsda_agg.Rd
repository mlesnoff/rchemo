\name{lwplsrda_agg}
\alias{lwplsrda_agg}
\alias{lwplslda_agg}
\alias{lwplsqda_agg}
\alias{predict.Lwplsrda_agg}
\alias{predict.Lwplsprobda_agg}
\encoding{latin1}

\title{Aggregation of KNN-LWPLSDA models with different numbers of LVs}

\description{

Ensemblist method where the predictions are calculated by "averaging" the predictions of KNN-LWPLSDA models built with different numbers of latent variables (LVs). 

For instance, if argument \code{nlv} is set to \code{nlv = "5:10"}, the prediction for a new observation is the most occurent level (vote) over the predictions returned by the models with 5 LVS, 6 LVs, ... 10 LVs, respectively.

- \code{lwplsrda_agg}: use \code{\link{plsrda}}.

- \code{lwplslda_agg}: use \code{\link{plslda}}.

- \code{lwplsqda_agg}: use \code{\link{plsqda}}.

}

\usage{

lwplsrda_agg(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    cri = 4,
    verb = FALSE
    ) 

lwplslda_agg(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv, 
    prior = c("unif", "prop"),
    cri = 4,
    verb = FALSE
    ) 

lwplsqda_agg(
    X, y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv, 
    prior = c("unif", "prop"),
    cri = 4,
    verb = FALSE
    ) 

\method{predict}{Lwplsrda_agg}(object, X, ...)

\method{predict}{Lwplsprobda_agg}(object, X, ...)

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}). \bold{Note:} If \code{y} is a factor, it is replaced by a character vector.}

\item{nlvdis}{The number of LVs to consider in the global PLS used for the dimension reduction before calculating the dissimilarities. If \code{nlvdis = 0}, there is no dimension reduction.}

\item{diss}{The type of dissimilarity used for defining the neighbors. Possible values are "eucl" (default; Euclidean distance), "mahal" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

\item{h}{A scale scalar defining the shape of the weight function. Lower is \eqn{h}, sharper is the function. See \code{\link{wdist}}.}

\item{k}{The number of nearest neighbors to select for each observation to predict.}

\item{nlv}{A character string such as "5:20" defining the range of the numbers of LVs to consider (here: the models with nb LVS = 5, 6, ..., 20 are averaged). Syntax such as "10" is also allowed (here: correponds to the single model with 10 LVs).}

\item{prior}{The prior probabilities of the classes. Possible values are "unif" (default; probabilities are set equal for all the classes) or "prop" (probabilities are set equal to the observed proportions of the classes in \code{y}).}

\item{cri}{Argument \code{cri} in function \code{\link{wdist}}.}

\item{verb}{Logical. If \code{TRUE}, fitting information are printed.}

\item{object}{A fitted model, output of a call to the main function.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\examples{

n <- 50 ; p <- 7
X <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
y <- sample(c(1, 4, 10), size = n, replace = TRUE)
#y <- sample(c("a", "10", "d"), size = n, replace = TRUE)
#y <- as.factor(sample(c(1, 4, 10), size = n, replace = TRUE))
#y <- as.factor(sample(c("a", "10", "d"), size = n, replace = TRUE))
Xtrain <- X ; ytrain <- y
m <- 5
Xtest <- X[1:m, ] ; ytest <- y[1:m]

############################# KNN-LWPLSRDA-AGG

nlvdis <- 5 ; diss <- "mahal"
h <- 2 ; k <- 10
nlv <- "2:6" 
fm <- lwplsrda_agg(
    Xtrain, ytrain, 
    nlvdis = nlvdis, diss = diss,
    h = h, k = k,
    nlv = nlv)
res <- predict(fm, Xtest)
res$pred
res$listnn

## Gridscore & gridcv
## Here, there is no sense to use gridscorelv & gridcvlv 
nlvdis <- 5 ; diss <- "mahal"
h <- c(2, Inf)
k <- c(10, 20)
nlv <- c("1:3", "2:5")
pars <- mpars(nlvdis = nlvdis, diss = diss,
              h = h, k = k, nlv = nlv)
pars

res <- gridscore(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, 
    fun = lwplsrda_agg, 
    pars = pars)
res

segm <- segmkf(n = n, K = 3, nrep = 1)
res <- gridcv(
    Xtrain, ytrain, 
    segm, score = err, 
    fun = lwplsrda_agg, 
    pars = pars,
    verb = TRUE)
names(res)
res$val

############################# KNN-LWPLSLDA-AGG

nlvdis <- 5 ; diss <- "mahal"
h <- 2 ; k <- 10
nlv <- "2:6" 
fm <- lwplslda_agg(
    Xtrain, ytrain, 
    nlvdis = nlvdis, diss = diss,
    h = h, k = k,
    nlv = nlv, prior = "prop")
res <- predict(fm, Xtest)
res$pred
res$listnn

nlvdis <- 5 ; diss <- "mahal"
h <- c(2, Inf)
k <- c(10, 20)
nlv <- c("1:3", "2:5")
pars <- mpars(nlvdis = nlvdis, diss = diss,
              h = h, k = k, nlv = nlv, 
              prior = c("unif", "prop"))
pars

res <- gridscore(
    Xtrain, ytrain, Xtest, ytest, 
    score = err, 
    fun = lwplslda_agg, 
    pars = pars)
res

segm <- segmkf(n = n, K = 3, nrep = 1)
res <- gridcv(
    Xtrain, ytrain, 
    segm, score = err, 
    fun = lwplslda_agg, 
    pars = pars,
    verb = TRUE)
names(res)
res$val

}

