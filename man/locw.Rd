\name{locw}
\alias{locw}
\alias{locwlv}

\encoding{latin1}

\title{Locally weighted models}

\description{

\code{locw} and \code{locwlv} are generic working functions returning predictions of KNN locally weighted (LW) models. One specific (= local) model is fitted for each observation to predict, and a prediction is returned. See the wrapper \code{\link{lwplsr}} (KNN-LWPLSR) for an example of use. 

In KNN-LW models, the prediction is built from two sequential steps, therafter referred to as \eqn{weighting "1"} and \eqn{weighting "2"}, respectively. For each new observation to predict, the two steps are as follow:

- \eqn{Weighting "1"}. The \eqn{k} nearest neighbors (in the training data set) are selected and the prediction model is fitted (in the next step) only on this neighborhood. It is equivalent to give a weight = 1 to the neighbors, and a weight = 0 to the other training observations, which corresponds to a binary weighting.

- \eqn{Weighting "2"}. Each of the \eqn{k} nearest neighbors eventually receives a weight (different from the usual \eqn{1/k}) before fitting the model. The weight depend from the dissimilarity (preliminary calculated) between the observation and the neighbor. This corresponds to a within-neighborhood weighting.

The prediction model used in step \eqn{"2"} has to be defined in a function specified in argument \code{fun}. If there are \eqn{m} new observations to predict, a list of \eqn{m} vectors defining the \eqn{m} neighborhoods has to be provided (argument \code{listnn}). Each of the \eqn{m} vectors contains the indexes of the nearest neighbors in the training set. The \eqn{m} vectors are not necessary of same length, i.e. the neighborhood size can vary between observations to predict. If there is a weighting in step \eqn{"2"}, a list of \eqn{m} vectors of weights have to be provided  (argument \code{listw}). Then  \code{locw} fits the model successively for each of the \eqn{m} neighborhoods, and returns the corresponding \eqn{m} predictions.

Function  \code{locwlv} is dedicated to prediction models based on latent variables (LVs) calculations, such as PLSR. It is much faster and recommended.  

}

\usage{

locw(Xtrain, Ytrain, X, listnn, listw = NULL, fun, verb = FALSE, ...)

locwlv(Xtrain, Ytrain, X, listnn, listw = NULL, fun, nlv, verb = FALSE, ...)
  
}

\arguments{

\item{Xtrain}{Training X-data (\eqn{n, p}).}

\item{Ytrain}{Training Y-data (\eqn{n, q}).}

\item{X}{New X-data (\eqn{m, p}) to predict.}

\item{listnn}{A list of \eqn{m} vectors defining weighting "1". Component \eqn{i} of this list is a vector (of length between 1 and \eqn{n}) of indexes. These indexes define the training observations that are the nearest neighbors of new observation \eqn{i}. Typically, \code{listnn} can be built from \code{\link{getknn}}, but any other list of length \eqn{m} can be provided. The \eqn{m} vectors can have equal length (i.e. the \eqn{m} neighborhoods are of equal size) or not (the number of neighbors varies between the observations to predict).}

\item{listw}{A list of \eqn{m} vectors defining weighting "2". Component \eqn{i} of this list is a vector (that must have the same length as component \eqn{i} of \code{listnn}) of the weights given to the nearest neighbors when the prediction model is fitted. Internally, weights are "normalized" to sum to 1 in each component. Default to \code{NULL} (weights are set to \eqn{1 / k} where \eqn{k}is the size of the neihborhodd).}

\item{fun}{A function corresponding to the prediction model to fit on the \eqn{m} neighborhoods.}

\item{nlv}{The number of LVs to calculate.}

\item{verb}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optional arguments to pass in function \code{fun}.}

}

\value{

See the examples.

}


\references{

Lesnoff M, Metz M, Roger J-M. Comparison of locally weighted PLS strategies for regression and discrimination on agronomic NIR data. Journal of Chemometrics. 2020;n/a(n/a):e3209. doi:10.1002/cem.3209.

}


\examples{

n <- 50 ; p <- 30
Xtrain <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
ytrain <- rnorm(n)
Ytrain <- cbind(ytrain, 100 * ytrain)
m <- 4
Xtest <- matrix(rnorm(m * p), ncol = p, byrow = TRUE)
ytest <- rnorm(m)
Ytest <- cbind(ytest, 10 * ytest)

k <- 5
z <- getknn(Xtrain, Xtest, k = k)
listnn <- z$listnn
listd <- z$listd
listnn
listd

listw <- lapply(listd, wdist, h = 2)
listw

nlv <- 2  
locw(Xtrain, Ytrain, Xtest, 
     listnn = listnn, fun = plskern, nlv = nlv)
locw(Xtrain, Ytrain, Xtest, 
     listnn = listnn, listw = listw, fun = plskern, nlv = nlv)

locwlv(Xtrain, Ytrain, Xtest, 
     listnn = listnn, listw = listw, fun = plskern, nlv = nlv)
locwlv(Xtrain, Ytrain, Xtest, 
     listnn = listnn, listw = listw, fun = plskern, nlv = 0:nlv)

}

\keyword{datagen}