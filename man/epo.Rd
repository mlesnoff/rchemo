\name{eposvd}
\alias{eposvd}
\encoding{latin1}

\title{External parameter orthogonalization (EPO)}

\description{

Pre-processing a X-dataset by external parameter orthogonalization (EPO; Roger et al 2003). The objective is to remove from a dataset X \eqn{(n, p)} some "detrimental" information (e.g. humidity effect) represented by a dataset \eqn{D (m, p)}. 

EPO consists in orthogonalizing the row observations of \eqn{X} to the detrimental sub-space defined by the first \eqn{nlv} non-centered PCA loadings vectors of \eqn{D}. 

Function \code{eposvd} uses a SVD factorization of \eqn{D} and returns \eqn{M (p, p)} the orthogonalization matrix, and \eqn{P} the considered loading vectors of \eqn{D}. 

The data corrected from the detrimental information \eqn{D} can be computed by \eqn{X_corrected = X * M}.

}

\usage{eposvd(D, nlv)}

\arguments{

\item{D}{A dataset  \eqn{(m, p)} containing detrimental information.}

\item{nlv}{The number of first loadings vectors of \eqn{D} considered for the orthogonalization.}

}

\value{See the examples.}

\references{

Roger, J.-M., Chauchard, F., Bellon-Maurel, V., 2003. EPO–PLS external parameter orthogonalisation of PLS application to temperature-independent measurement of sugar content of intact fruits. Chemometrics and Intelligent Laboratory Systems 66, 191–204. https://doi.org/10.1016/S0169-7439(03)00051-0

Roger, J.-M., Boulet, J.-C., 2018. A review of orthogonal projections for calibration. Journal of Chemometrics 32, e3045. https://doi.org/10.1002/cem.3045

}


\examples{

n <- 4 ; p <- 8 
X <- matrix(rnorm(n * p), ncol = p)
m <- 3
D <- matrix(rnorm(m * p), ncol = p)    # Detrimental information

nlv <- 2
res <- eposvd(D, nlv = nlv)
M <- res$M       # orthogonalization matrix
P <- res$P       # detrimental directions matrix (loadings of D = columns of P)
M
P

## The matrix corrected from D can be computed by:
## X_corr <- X %*% M    
## Rows of the corrected matrix X_corr
## are orthogonal to the loadings vectors (columns of P):
## X_corr %*% P 

}

\keyword{datagen}