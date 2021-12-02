\name{sampcla}
\alias{sampcla}
\encoding{latin1}

\title{Within-class sampling}

\description{

The function divides a datset in two sets, "train" vs "test", using a stratified sampling on defined classes.

If argument \code{y = NULL} (default), the sampling is random within each class. If not, the sampling is systematic (regular grid) within each class over the quantitative variable \eqn{y}.

}

\usage{

sampcla(x, y = NULL, m)

}

\arguments{

\item{x}{A vector (length \eqn{m}) defining the class membership of the observations.}

\item{y}{A vector (length \eqn{m}) defining the quantitative variable for the systematic sampling. If \code{NULL} (default), the sampling is random within each class.}

\item{m}{Either an integer defining the equal number of test observation(s) to select per class, or a vector of integers defining the numbers to select for each class. In the last case, vector \code{m} must have a length equal to the number of classes present in \code{x}, and be ordered in the same way as the ordered class membership.}

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

sampcla(x, m = 2)
s <- sampcla(x, m = 2)$test
x[s]

sampcla(x, m = c(1, 2, 1))
s <- sampcla(x, m = c(1, 2, 1))$test
x[s]

y <- rnorm(length(x))
sampcla(x, y, m = 2)
s <- sampcla(x, y, m = 2)$test
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

psamp <- .20
m <- round(psamp * N * p)
m

## Random

res <- sampcla(x, m = m)
s <- res$test
table(x[s])

## Systematic for y

res <- sampcla(x, y, m = m)
s <- res$test
table(x[s])

}

\keyword{datagen}