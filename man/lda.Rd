\name{lda}
\alias{lda}
\alias{qda}
\alias{predict.Lda}
\alias{predict.Qda}
\encoding{latin1}

\title{LDA and QDA}

\description{
Probabilistic (parametric) linear and quadratic discriminant analysis.

For each observation to predict, the posterior probability to belong to a given class is estimated using the Bayes' formula, assuming priors (proportional or uniform) and a multivariate Normal distribution for the dependent variables \eqn{X}. The prediction is the class with the highest posterior probability.

LDA assumes homogeneous \eqn{X-}covariance matrices for the classes while QDA assumes different covariance matrices. The functions use \code{\link{dmnorm}} for estimating the multivariate Normal densities.  
}

\usage{

lda(X, y, prior = c("unif", "prop"))

qda(X, y, prior = c("unif", "prop"))
  
\method{predict}{Lda}(object, X, ...)  
\method{predict}{Qda}(object, X, ...)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For the auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{y}{Training class membership (\eqn{n}). \bold{Note:} If \code{y} is a factor, it is replaced by a character vector.}

\item{prior}{The prior probabilities of the classes. Possible values are "unif" (default; probabilities are set equal for all the classes) or "prop" (probabilities are set equal to the observed proportions of the classes in \code{y}).}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments. Not used.}

}

\value{

See the examples.

}

\references{

Saporta, G., 2011. Probabilités analyse des données et statistique. Editions Technip, Paris, France.

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.

}

\examples{

data(iris)

X <- iris[, 1:4]
y <- iris[, 5]
N <- nrow(X)

m <- round(.25 * N) # Test
n <- N - m          # Training
s <- sample(1:N, m)
Xtrain <- X[-s, ]
ytrain <- y[-s]
Xtest <- X[s, ]
ytest <- y[s]

prior <- "unif"
#prior <- "prop"

fm <- lda(Xtrain, ytrain, prior = prior)
res <- predict(fm, Xtest)
names(res)

headm(res$pred)
headm(res$ds)
headm(res$posterior)

err(res$pred, ytest)

}

\keyword{datagen}