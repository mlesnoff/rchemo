\name{svmr}
\alias{svmr}
\alias{svmda}
\alias{predict.Svm}
\alias{summary.Svm}
\encoding{latin1}

\title{SVM Regression and Discrimination}

\description{

SVM models with Gaussian (RBF) kernel

\code{svmr}: SVM regression (SVMR).

\code{svmda}: SVM discrimination (SVMC).

The functions uses function \code{\link{svm}} of package \code{e1071} (Meyer et al. 2021) available on CRAN (e1071 uses the tool box LIVSIM; Chang & Lin, http://www.csie.ntu.edu.tw/~cjlin/libsvm). 

The SVM models are fitted with parameterization \eqn{'C'}, not the \eqn{'nu'} parameterization. 

The Gaussian kernel is defined by: \eqn{exp(-.5 * euclsq(X, Y) / sigma^2)}.

For tuning the model, usual preliminary ranges are for instance:

- C = 10^(-5:15)

- epsilon = seq(.1, .3, by = .1)

- sigma = 10^(-6:3)

}

\usage{

svmr(X, y, C = 1, epsilon = .1, sigma = 1, scale = FALSE)

svmda(X, y, C = 1, epsilon = .1, sigma = 1, scale = FALSE)

\method{predict}{Svm}(object, X, ...)  

\method{summary}{Svm}(object, ...)  

}

\arguments{

\item{X}{For the main function: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training Y-data (\eqn{n}).}

\item{C}{The cost of constraints violation \eqn{C} parameter. See \code{\link{svm}}.}

\item{epsilon}{The \eqn{epsilon} parameter in the insensitive-loss function. See \code{\link{svm}}.}

\item{sigma}{The \eqn{sigma} parameter in the RBF kernel. In \code{\link{svm}}, parameter \code{gamma = 1 / (2 * sigma^2)}.}

\item{scale}{Logical. If \code{TRUE}, \code{X} and \code{Y} are scaled internally.}

\item{object}{A fitted model, output of a call to the main function.}

\item{...}{Optional arguments.}

}

\value{See the examples.}

\references{

Meyer, M. 2021 Support Vector Machines - The Interface to libsvm in package e1071. FH Technikum Wien, Austria, David.Meyer@R-Project.org. https://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf

Chang, C.-C. & Lin, C.-J. (2001). LIBSVM: a library for support vector machines. Software available at http://www.csie.ntu.edu.tw/~cjlin/libsvm. Detailed documentation (algorithms, formulae, . . . ) can be found in http://www.csie.ntu.edu.tw/~cjlin/papers/libsvm.ps.gz

}

\examples{

#################################### SVMR

n <- 50 ; p <- 4
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- rnorm(n)
m <- 3
Xtest <- Xtrain[1:m, , drop = FALSE] 
ytest <- ytrain[1:m]

fm <- svmr(Xtrain, ytrain)
predict(fm, Xtest)

pred <- predict(fm, Xtest)$pred
msep(pred, ytest)

summary(fm)

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

fm <- svmr(X, y, sigma = 3)
pred <- predict(fm, X)$pred
plot(X, y, type = "p")
lines(X, zy, lty = 2)
lines(X, pred, col = "red")

#################################### SVMC

n <- 50 ; p <- 8
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- sample(c(1, 4, 10), size = n, replace = TRUE)
#ytrain <- sample(c("a", "10", "d"), size = n, replace = TRUE)
#ytrain <- as.factor(sample(c(1, 4, 10), size = n, replace = TRUE))
#ytrain <- as.factor(sample(c("a", "10", "d"), size = n, replace = TRUE))
m <- 5
Xtest <- Xtrain[1:m, ] ; ytest <- ytrain[1:m]

C <- 100 ; epsilon <- .1 ; sigma <- 1 
fm <- svmda(Xtrain, ytrain,
    C = C, epsilon = epsilon, sigma = sigma)
predict(fm, Xtest)

pred <- predict(fm, Xtest)$pred
err(pred, ytest)

summary(fm)

}

\keyword{datagen}