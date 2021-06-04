\name{rr}
\alias{rr}
\alias{coef.Rr}
\alias{predict.Rr}
\encoding{latin1}

\title{Linear Ridge Regression}

\description{
Fitting linear ridge regression models (RR) (Hoerl & Kennard 1970, Hastie & Tibshirani 2004, Hastie et al 2009, Cule & De Iorio 2012) by SVD factorization.
}

\usage{

rr(X, Y, weights = NULL, lb = 1e-2)

\method{coef}{Rr}(object, ..., lb = NULL)  

\method{predict}{Rr}(object, X, ..., lb = NULL)  

}

\arguments{

\item{X}{For the main function: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{weights}{Weights (\eqn{n, 1}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{lb}{A value of regularization parameter \eqn{lambda}. If \code{lb = 0}, a pseudo-inverse is used.}

\item{object}{A fitted model, output of a call to the main function.}

\item{...}{Optional arguments. Not used.}

}

\value{See the examples.}

\references{

Cule, E., De Iorio, M., 2012. A semi-automatic method to guide the choice of ridge parameter in ridge regression. arXiv:1205.0686.

Hastie, T., Tibshirani, R., 2004. Efficient quadratic regularization for expression arrays. Biostatistics 5, 329-340. https://doi.org/10.1093/biostatistics/kxh010

Hastie, T., Tibshirani, R., Friedman, J., 2009. The elements of statistical learning: data mining, inference, and prediction, 2nd ed. Springer, New York.

Hoerl, A.E., Kennard, R.W., 1970. Ridge Regression: Biased Estimation for Nonorthogonal Problems. Technometrics 12, 55-67. https://doi.org/10.1080/00401706.1970.10488634

Wu, W., Massart, D.L., de Jong, S., 1997. The kernel PCA algorithms for wide data. Part I: Theory and algorithms. Chemometrics and Intelligent Laboratory Systems 36, 165-172. https://doi.org/10.1016/S0169-7439(97)00010-5

}

\examples{

n <- 6 ; p <- 4
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- rnorm(n)
Ytrain <- cbind(y1 = ytrain, y2 = 100 * ytrain)
m <- 3
Xtest <- Xtrain[1:m, , drop = FALSE] 
Ytest <- Ytrain[1:m, , drop = FALSE] ; ytest <- Ytest[1:m, 1]

lb <- .1
fm <- rr(Xtrain, Ytrain, lb = lb)
coef(fm)
coef(fm, lb = .8)
predict(fm, Xtest)
predict(fm, Xtest, lb = c(0.1, .8))

pred <- predict(fm, Xtest)$pred
msep(pred, Ytest)

}

\keyword{datagen}