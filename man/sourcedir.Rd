\name{sourcedir}
\alias{sourcedir}
\encoding{latin1}

\title{Source R functions in a directory}

\description{Source all the R functions contained in a directory.} 

\usage{sourcedir(path, trace = TRUE, ...)}

\arguments{

\item{path}{A character vector of full path names; the default corresponds to the working directory, \code{getwd()}.}

\item{trace}{Logical. Default to \code{TRUE}. See the code.}

\item{...}{Additional arguments to pass in the function \code{\link{list.files}}.}

}

\value{

Sourcing.

}


\examples{

path <- "D:/Users/Fun"
#sourcedir(path, FALSE)

}

\keyword{datagen}