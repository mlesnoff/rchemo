\name{sampclas}
\alias{sampclas}
\encoding{latin1}

\title{Within-class sampling}

\description{

The function divides a datset in two sets, "train" vs "test", using a stratified sampling on pre-defined classes.

If argument \code{y = NULL} (default), the sampling is random within each class. If not, the sampling is systematic (regular grid) over the quantitative variable \eqn{y}.

}

\usage{

sampclas(x, y = NULL, n)

}

\arguments{

\item{x}{A vector (\eqn{n}) defining the class membership of the observations.}

\item{y}{A vector (\eqn{n}) defining the quantitative variable for the systematic sampling. If \code{NULL} (default), the sampling is random within each class.}

\item{n}{Either an integer defining the equal number of training observation(s) to select per class, or a vector of integers defining the numbers to select for each class. In the last case, vector \code{n} must have a length equal to the number of classes present in \code{x}, and be ordered in the same way as the ordered class membership.}

}

\value{

Indexes (i.e. position in \eqn{x}) of the selected observations.

}

\references{

Naes, T., 1987. The design of calibration in near infra-red reflectance analysis by clustering. Journal of Chemometrics 1, 121-134.

}

\examples{

x <- sample(c(1, 3, 4), size = 20, replace = TRUE)
#x <- sample(c("B", "3", "a"), size = 20, replace = TRUE)
#x <- as.factor(sample(c("B", "3", "a"), size = 20, replace = TRUE))
table(x)

sampclas(x, n = 2)
s <- sampclas(x, n = 2)$train
x[s]

sampclas(x, n = c(1, 2, 1))
s <- sampclas(x, n = c(1, 2, 1))$train
x[s]

y <- rnorm(length(x))
sampclas(x, y, n = 2)
s <- sampclas(x, y, n = 2)$train
x[s]

########## Representative stratified sampling from an unsupervised clustering

data(cassav)
X <- cassav$Xtrain
y <- cassav$ytrain
N <- nrow(X)

fm <- pcaeigenk(X, nlv = 10)
z <- stats::kmeans(x = fm$T, centers = 3, nstart = 25, iter.max = 50)
x <- z$cluster
z <- table(x)
z
p <- c(z) / N
p

psamp <- .70
n <- round(psamp * N * p)
n

## Random

res <- sampclas(x, n = n)
s <- res$train
table(x[s])

## Systematic for y

res <- sampclas(x, y, n = n)
s <- res$train
table(x[s])

}

\keyword{datagen}