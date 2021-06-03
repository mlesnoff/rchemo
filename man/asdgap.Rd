\name{asdgap}
\alias{asdgap}
\docType{data}
\encoding{latin1}

\title{asdgap}

\description{ASD NIRS dataset, with gaps in the spectra at wawelengths = 1000 and 1800 nm.}

\usage{data(asdgap)}

\format{
A list with the following component: 
  
X: 5 spectra.
}


\references{
Thanks to J.-F. Roger (Inrae, France) and M. Ecarnot (Inrae, France) for the method.
}

\examples{

data(asdgap)
names(asdgap)
X <- asdgap$X

numcol <- which(colnames(X) == "1000" | colnames(X) == "1800")
numcol
plotsp(X, lwd = 1.5)
abline(v = as.numeric(colnames(X)[1]) + numcol - 1, col = "grey", lty = 3)

}


\keyword{datasets}
