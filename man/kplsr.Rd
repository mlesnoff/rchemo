\name{kplsr}
\alias{kplsr}
\alias{transform.Kpls}
\alias{coef.Kpls}
\alias{predict.Kplsr}
\encoding{latin1}

\title{KPLSR Models}

\description{

NIPALS Kernel PLSR algorithm such as described in Rosipal & Trejo (2001). 

The algorithm is slow for \eqn{n >= 500}.
}

\usage{

kplsr(X, Y, nlv, kern = "krbf", weights = NULL,
     tol = .Machine$double.eps^0.5, maxit = 100, ...)

\method{transform}{Kpls}(object, X, ..., nlv = NULL)  

\method{coef}{Kpls}(object, ..., nlv = NULL)  

\method{predict}{Kplsr}(object, X, ..., nlv = NULL)  

}

\arguments{

\item{X}{For the main function: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{nlv}{The number(s) of LVs to calculate.}

\item{kern}{Name of the function defining the considered kernel for building the Gram matrix. See \code{\link{krbf}} for syntax, and other available kernel functions.}

\item{weights}{Weights (\eqn{n, 1}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{object}{A fitted model, output of a call to the main function.}

\item{tol}{Tolerance level for stopping the NIPALS iterations.}

\item{maxit}{Maximum number of NIPALS iterations.}

\item{...}{Optionnal arguments to pass in the kernel function defined in \code{kern} (e.g. \code{sigma} for \code{\link{krbf}}).}

}

\value{See the examples.}


\references{

Rosipal, R., Trejo, L.J., 2001. Kernel Partial Least Squares Regression in Reproducing Kernel Hilbert Space. Journal of Machine Learning Research 2, 97-123.

}

\examples{

n <- 6 ; p <- 4
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- rnorm(n)
Ytrain <- cbind(y1 = ytrain, y2 = 100 * ytrain)
m <- 3
Xtest <- Xtrain[1:m, , drop = FALSE] 
Ytest <- Ytrain[1:m, , drop = FALSE] ; ytest <- Ytest[1:m, 1]

nlv <- 2
fm <- kplsr(Xtrain, Ytrain, nlv = nlv, kern = "krbf", sigma = .8)
transform(fm, Xtest)
transform(fm, Xtest, nlv = 1)
coef(fm)
coef(fm, nlv = 1)

predict(fm, Xtest)
predict(fm, Xtest, nlv = 0:nlv)$pred

pred <- predict(fm, Xtest)$pred
msep(pred, Ytest)

nlv <- 2
fm <- kplsr(Xtrain, Ytrain, nlv = nlv, kern = "kpol", degree = 2, offset = 10)
predict(fm, Xtest, nlv = nlv)

sigma <- 10^(-1:1)
nlv <- 2
pars <- expand.grid(
    sigma = sigma,
    nlv = 0:nlv
    )
res <- gridscorelv(
    Xtrain, ytrain, Xtest, ytest, 
    score = msep, 
    fun = kplsr, 
    pars = pars,
    verb = TRUE
    )
res

####### Example of fitting the function sinc(x)
####### described in Rosipal & Trejo 2001 p. 105-106 

x <- seq(-10, 10, by = .2)
x[x == 0] <- 1e-5
n <- length(x)
zy <- sin(abs(x)) / abs(x)
y <- zy + rnorm(n, 0, .2)
plot(x, y, type = "p")
lines(x, zy, lty = 2)
X <- matrix(x, ncol = 1)

nlv <- 2
fm <- kplsr(X, y, nlv = nlv)
pred <- predict(fm, X)$pred
plot(X, y, type = "p")
lines(X, zy, lty = 2)
lines(X, pred, col = "red")

}

\keyword{datagen}