\name{checkna}
\alias{checkna}
\encoding{latin1}

\title{Find and count NA values in a dataset}

\description{Find and count NA values in each row observation of a dataset.}

\usage{checkna(X)}

\arguments{
\item{X}{A dataset.}
}

\value{A data frame summarizing the numbers of NA by rows.}

\examples{

X <- data.frame(
  v1 = c(NA, rnorm(9)), 
  v2 = c(NA, rnorm(8), NA),
  v3 = c(NA, NA, NA, rnorm(7))
)
X

checkna(X)

}

\keyword{datagen}