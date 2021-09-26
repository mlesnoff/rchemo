\name{lwplsr}
\alias{lwplsr}
\alias{predict.Lwplsr}

\encoding{latin1}

\title{KNN-LWPLSR}

\description{

Function \code{lwplsr} fits KNN-LWPLSR models described in Lesnoff et al. (2020). The function uses functions \code{\link{getknn}}, \code{\link{locw}} and PLSR functions. See the code for details. Many variants of such pipelines can be build using \code{\link{locw}}.

- LWPLSR: This is a particular case of "weighted PLSR" (WPLSR) (e.g. Schaal et al. 2002). In WPLSR, a priori weights, different from the usual \eqn{1/n} (standard PLSR), are given to the \eqn{n} training observations. These weights are used for calculating (i) the PLS scores and loadings and (ii) the regression model of the response(s) over the scores (by weighted least squares). LWPLSR is a particular case of WPLSR. "L" comes from "localized": the weights are defined from dissimilarities (e.g. distances) between the new observation to predict and the training observations. By definition of LWPLSR, the weights, and therefore the fitted WPLSR model, change for each new observation to predict.

- KNN-LWPLSR: Basic versions of LWPLSR (e.g. Sicard & Sabatier 2006, Kim et al 2011) use, for each observation to predict, all the \eqn{n} training observation. This can be very time consuming, in particular for large \eqn{n}. A faster and often more efficient strategy is to preliminary select, in the training set, a number of \eqn{k} nearest neighbors to the observation to predict (this is referred to as \eqn{"weighting 1"} in function \code{\link{locw}}) and then to apply LWPLSR only to this pre-selected neighborhood (this is referred to as\eqn{weighting "2"} in \code{\link{locw}}). This strategy corresponds to KNN-LWPLSR. 

In function \code{lwplsr}, the dissimilarities used for computing the weights can be calculated from the original X-data or after a dimension reduction (argument \code{nlvdis}). In the last case, global PLS scores are computed from \eqn{(X, Y)} and the dissimilarities are calculated on these scores. For high dimension X-data, the dimension reduction is in general required for using the Mahalanobis distance.   

}

\usage{

lwplsr(X, Y,
    nlvdis, diss = c("eucl", "mahal"),
    h, k,
    nlv,
    cri = 4,
    verb = FALSE)

\method{predict}{Lwplsr}(object, X, ..., nlv = NULL)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{nlvdis}{The number of LVs to consider in the global PLS used for the dimension reduction before calculating the dissimilarities. If \code{nlvdis = 0}, there is no dimension reduction.}

\item{diss}{The type of dissimilarity used for defining the neighbors. Possible values are "eucl" (default; Euclidean distance), "mahal" (Mahalanobis distance), or "correlation". Correlation dissimilarities are calculated by sqrt(.5 * (1 - rho)).}

\item{h}{A scale scalar defining the shape of the weight function. Lower is \eqn{h}, sharper is the function. See \code{\link{wdist}}.}

\item{k}{The number of nearest neighbors to select for each observation to predict.}

\item{nlv}{The number(s) of LVs to calculate in the local PLSR models.}

\item{cri}{Argument \code{cri} in function \code{\link{wdist}}.}

\item{verb}{Logical. If \code{TRUE}, fitting information are printed.}

\item{object}{A fitted model, output of a call to the main function.}

\item{...}{Optional arguments.}

}

\value{See the examples. Not used.}

\references{

Kim, S., Kano, M., Nakagawa, H., Hasebe, S., 2011. Estimation of active pharmaceutical ingredients content using locally weighted partial least squares and statistical wavelength selection. Int. J. Pharm., 421, 269-274.

Lesnoff, M., Metz, M., Roger, J.-M., 2020. Comparison of locally weighted PLS strategies for regression and discrimination on agronomic NIR data. Journal of Chemometrics, e3209. https://doi.org/10.1002/cem.3209

Schaal, S., Atkeson, C., Vijayamakumar, S. 2002. Scalable techniques from nonparametric statistics for the real time robot learning. Applied Intell., 17, 49-60.

Sicard, E. Sabatier, R., 2006. Theoretical framework for local PLS1 regression and application to a rainfall data set. Comput. Stat. Data Anal., 51, 1393-1410.

}

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
nlv <- 2  
fm <- lwplsr(
    Xtrain, Ytrain, 
    nlvdis = nlvdis, diss = diss,
    h = h, k = k,
    nlv = nlv)
res <- predict(fm, Xtest)
names(res)
res$pred
msep(res$pred, Ytest)

res <- predict(fm, Xtest, nlv = 0:2)
res$pred

}

