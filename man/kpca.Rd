\name{kpca}
\alias{kpca}
\alias{transform.Kpca}
\alias{summary.Kpca}
\encoding{latin1}

\title{KPCA}

\description{

Kernel PCA (Scholkopf et al. 1997, Scholkopf & Smola 2002, Tipping 2001).

}

\usage{

kpca(X, nlv, kern = "krbf", weights = NULL, ...)

\method{transform}{Kpca}(object, X, ..., nlv = NULL)  

\method{summary}{Kpca}(object, X = NULL, ...)  

}

\arguments{

\item{X}{For the main functions and auxiliary function \code{summary}: Training X-data (\eqn{n, p}). In \code{summary}, \code{X} is only required for variable coordinates and contributions, and the correlation circle (Default to \code{NULL}). --- For the other auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{nlv}{The number of PCs to calculate.}

\item{weights}{Weights (\eqn{n, 1}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{kern}{Name of the function defining the considered kernel for building the Gram matrix. See \code{\link{krbf}} for syntax, and other available kernel functions.}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments to pass in the kernel function defined in \code{kern} (e.g. \code{sigma} for \code{\link{krbf}}).}

}

\value{See the examples.}

\references{

Bennett, K.P., Embrechts, M.J., 2003. An optimization perspective on kernel partial least squares regression, in: Advances in Learning Theory: Methods, Models and Applications, NATO Science Series III: Computer & Systems Sciences. IOS Press Amsterdam, pp. 227-250.

Rosipal, R., Trejo, L.J., 2001. Kernel Partial Least Squares Regression in Reproducing Kernel Hilbert Space. Journal of Machine Learning Research 2, 97-123.

Scholkopf, B., Smola, A., Müller, K.-R., 1997. Kernel principal component analysis, in: Gerstner, W., Germond, A., Hasler, M., Nicoud, J.-D. (Eds.), Artificial Neural Networks — ICANN 97, Lecture Notes in Computer Science. Springer, Berlin, Heidelberg, pp. 583-588. https://doi.org/10.1007/BFb0020217

Scholkopf, B., Smola, A.J., 2002. Learning with kernels: support vector machines, regularization, optimization, and beyond, Adaptive computation and machine learning. MIT Press, Cambridge, Mass.

Tipping, M.E., 2001. Sparse kernel principal component analysis. Advances in neural information processing systems, MIT Press. http://papers.nips.cc/paper/1791-sparse-kernel-principal-component-analysis.pdf

}

\examples{

n <- 5 ; p <- 4
X <- matrix(rnorm(n * p), ncol = p)

nlv <- 3
kpca(X, nlv = nlv, kern = "krbf")

fm <- kpca(X, nlv = nlv, kern = "krbf", sigma = .6)
fm$T
transform(fm, X[1:2, ])
transform(fm, X[1:2, ], nlv = 1)
summary(fm)

## Usual PCA
nlv <- 3
pcaeigen(X, nlv = nlv)$T
kpca(X, nlv = nlv, kern = "kpol")$T

}

\keyword{datagen}