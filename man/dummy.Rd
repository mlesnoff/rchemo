\name{dummy}
\alias{dummy}
\encoding{latin1}

\title{Table of dummy variables}

\description{

The function builds a table of dummy variables from a qualitative variable. A binary (i.e. 0/1) variable is created for each level of the qualitative variable. 

}

\usage{dummy(y)}

\arguments{

\item{y}{A qualitative variable.}

}

\value{A matrix of dummy variables (i.e. binary variables), each representing a given level of the qualitative variable.}


\examples{

y <- c(1, 1, 3, 2, 3)
dummy(y)

y <- c("B", "a", "B")
dummy(y)
dummy(as.factor(y))

}

\keyword{datagen}