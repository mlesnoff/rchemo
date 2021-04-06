\name{dtagg}
\alias{dtagg}
\encoding{latin1}

\title{Summary statistics of data subsets}

\description{

Faster alternative to \code{\link{aggregate}} to calculate a summary statistic over data subsets.  \code{dtagg} uses function \code{\link{data.table}} of package \code{data.table}. 

}

\usage{

dtagg(formula, data, FUN = mean, ...)

}

\arguments{

\item{formula}{A left and right-hand-sides formula defing the variable and the aggregation levels on which is calculated the statistic.}

\item{data}{A dataframe.}

\item{FUN}{Function defining the statistic to compute (default to \code{mean}).}

\item{...}{Eventual additional arguments to pass through \code{FUN}.}

}

\value{

A dataframe, see the examples.

}


\examples{

dat <- data.frame(matrix(rnorm(2 * 100), ncol = 2))
names(dat) <- c("y1", "y2")
dat$typ1 <- sample(1:2, size = nrow(dat), TRUE)
dat$typ2 <- sample(1:3, size = nrow(dat), TRUE)

headm(dat)

dtagg(y1 ~ 1, data = dat)

dtagg(y1 ~ typ1 + typ2, data = dat)

dtagg(y1 ~ typ1 + typ2, data = dat, trim = .2)

}


\keyword{datagen}