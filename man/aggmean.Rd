\name{aggmean}
\alias{aggmean}
\encoding{latin1}

\title{Centers of classes}

\description{
Calculation of the centers (means) of classes of row observations of a data set. 
}

\usage{aggmean(X, y = NULL)}

\arguments{

\item{X}{Data (\eqn{n, p}) for which are calculated the centers (column-wise means).}

\item{y}{Class membership (\eqn{n, 1}) of the row of \code{X}. Default to \code{NULL} (all the rows of are considered).}

}

\value{

A list of outputs.

}


\examples{

n <- 8 ; p <- 6
X <- matrix(rnorm(n * p, mean = 10), ncol = p, byrow = TRUE)
y <- sample(1:2, size = n, replace = TRUE)
aggmean(X, y)

data(forages)
Xtrain <- forages$Xtrain
ytrain <- forages$ytrain
table(ytrain)
u <- aggmean(Xtrain, ytrain)$ct
headm(u)
plotsp(u, col = 1:4, main = "Means")
x <- Xtrain[1:20, ]
plotsp(x, ylab = "Absorbance", col = "grey")
u <- aggmean(x)$ct
plotsp(u, col = "red", add = TRUE, lwd = 2)

}

\keyword{datagen}