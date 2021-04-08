\name{summ}
\alias{summ}
\encoding{latin1}

\title{Description of the quantitative variables of a data set}

\description{
Displays summary statistics for each quantitative column of the data set.
}

\usage{summ(X, nam = NULL, digits = 3)}

\arguments{

\item{X}{A matrix or data frame containing the variables to summarize.}

\item{nam}{Names of the variables to summarize (vector of character strings). Default to \code{NULL} (all the columns are considered).}

\item{digits}{Number of digits for the numerical outputs.}

}

\value{A dataframe of summary statistics.}

\examples{

dat <- data.frame(
  v1 = rnorm(10),
  v2 = c(NA, rnorm(8), NA),
  v3 = c(NA, NA, NA, rnorm(7))
  )
dat

summ(dat)
summ(dat, nam = c("v1", "v3"))

}

\keyword{datagen}