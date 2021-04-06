\name{plsda}
\alias{plsda}
\alias{plslda}
\alias{plsqda}
\alias{predict.PlsDa}
\alias{predict.PlsDaProb}
\encoding{latin1}

\title{PLSDA models}

\description{

Discrimination (DA) based on PLS latent variables (LVs).

The training class membership variable \eqn{y} (univariatel) is firstly transformed (with function \code{\link{dummy}}) to a dummy table containing \eqn{nclas} columns, where \eqn{nclas} is the number of classes present in \eqn{y}. Each column is a dummy variable (0/1). Then, a PLS2 is implemented on the \eqn{X-}data and the dummy table, returning LVs that are used as dependent variables in a DA model.

- \code{plsda} fits the usual "PLDA": the final prediction (a given class) corresponds to the dummy variable for which the prediction (unbounded probability estimate) is the highest.

- \code{plslda} and \code{plsqda} fit a PLS-LDA and a PLS-QDA, respectively: \code{\link{lda}} and \code{\link{qda}} are run over the PLS2 LVs.

The traing matrix \eqn{X} is centered before the analyses, but \eqn{X} is not column-wise scaled (there is no argument \code{scale} available). If a scaling is needed, the user has to scale \eqn{X} before using the functions. 

The row observations can eventually be weighted in the PLS2, using argument \code{weights}.

}

\usage{

plsda(X, y, nlv, weights = NULL)

plslda(X, y, nlv, weights = NULL, prior = c("unif", "prop"))

plsqda(X, y, nlv, weights = NULL, prior = c("unif", "prop"))

\method{predict}{PlsDa}(fm, X, ..., nlv = NULL) 

\method{predict}{PlsDaProb}(fm, X, ..., nlv = NULL) 

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}).}

\item{nlv}{The number(s) of LVs to calculate.}

\item{weights}{Weights (\eqn{n}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{prior}{The prior probabilities of the classes. Possible values are "unif" (default; probabilities are set equal for all the classes) or "prop" (probabilities are set equal to the observed proportions of the classes in \code{y}).}

\item{fm}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments.}

}

\value{

See the examples.

}

\examples{

n <- 50 ; p <- 30
X <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
y <- sample(c(1, 4, 10), size = n, replace = TRUE)
#y <- sample(c("a", "10", "d"), size = n, replace = TRUE)
z <- y
#z <- as.factor(y)
Xtrain <- X ; ytrain <- z
Xtest <- X[1:5, ] ; ytest <- z[1:5]

nlv <- 5

fm <- plsda(Xtrain, ytrain, nlv = nlv)
predict(fm, Xtest)
coef(fm)
transform(fm, X[1:2, ])

fm <- plslda(Xtrain, ytrain, nlv = nlv)
predict(fm, Xtest)
coef(fm[[1]])
transform(fm[[1]], X[1:2, ])

}

\keyword{datagen}