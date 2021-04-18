\name{pcasvd}
\alias{pcasvd}
\alias{pcaeigen}
\alias{pcaeigenk}
\alias{pcanipals}
\alias{pcanipalsna}
\alias{transform.Pca}
\alias{summary.Pca}
\encoding{latin1}

\title{PCA algorithms}

\description{

Algorithms fitting a centered PCA of a matrix \eqn{X}. 

Noting \eqn{D} a \eqn{n, n} diagonal matrix of weights for the observations (rows of \eqn{X}), the functions consist in:

- \code{pcasvd}: SVD decomposition of \eqn{D^(1/2) * X}, using function \code{\link{svd}}. 

- \code{pcaeigen}:Eigen decomposition of \eqn{X' * D * X}, using function \code{\link{eigen}}. 

- \code{pcaeigenk}: Eigen decomposition of \eqn{D^(1/2) *, * X' D^(1/2)}, using function \code{\link{eigen}}. This is the "kernel cross-product trick" version of the PCA algorithm (Wu et al. 1997). For wide matrices (\eqn{n << p}) and \eqn{n} not too large, this algorithm can be much faster than the others. 

- \code{pcanipals}: Eigen decomposition of \eqn{X' * D * X} using NIPALS. 

- \code{pcanipalsna}: Eigen decomposition of \eqn{X' * D * X} using NIPALS allowing missing data in \eqn{X}. 


Function \code{pcanipalsna} accepts missing data (\code{NA}s) in \eqn{X}, unlike the other functions. The part of \code{pcanipalsna} accounting specifically for missing missing data is based on the efficient code of K. Wright in the R package \code{nipals} (https://cran.r-project.org/web/packages/nipals/index.html).

\bold{Gram-Schmidt orthogonalization in the NIPALS algorithm}

The PCA NIPALS is known to generate a loss of orthogonality of the PCs (due to the accumulation of rounding errors in the successive iterations), particularly for large matrices or with high degrees of column collinearity.

With missing data, orthogonality of loadings is not satisfied neither.

An approach for coming back to orthogonality (PCs and loadings) is the iterative classical Gram-Schmidt orthogonalization (Lingen 2000, Andrecut 2009, and vignette of R package \code{nipals}), referred to as the iterative CGS. It consists in adding a CGS orthorgonalization step in each iteration of the PCs and loadings calculations.

For the case with missing data, the iterative CGS does not insure that the orthogonalized PCs are centered.

\bold{Auxiliary function}

\code{transform} Calculates the PCs for any new matrix \eqn{X} from the model.

\code{summary} returns summary information for the model.

}

\usage{

pcasvd(X, nlv, weights = NULL)

pcaeigen(X, nlv, weights = NULL)

pcaeigenk(X, nlv, weights = NULL)

pcanipals(X, nlv, weights = NULL,
    gs = TRUE, 
    tol = .Machine$double.eps^0.5, maxit = 200)

pcanipalsna(X, nlv, 
    gs = TRUE,
    tol = .Machine$double.eps^0.5, maxit = 200)
  
\method{transform}{Pca}(object, X, ..., nlv = NULL)  

\method{summary}{Pca}(object, X = NULL, ...)  
  
}

\arguments{

\item{X}{For the main functions and auxiliary function \code{summary}: Training X-data (\eqn{n, p}). In \code{summary}, \code{X} is only required for variable coordinates and contributions, and the correlation circle (Default to \code{NULL}). --- For the other auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{nlv}{The number of PCs to calculate.}

\item{weights}{Weights (\eqn{n, 1}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments.}

\bold{Specific for the NIPALS algorithm}

\item{gs}{Logical indicating if a Gram-Schmidt orthogonalization is implemented or not (default to \code{TRUE}).}

\item{tol}{Tolerance for testing convergence of the NIPALS iterations for each PC.}

\item{maxit}{Maximum number of NIPALS iterations for each PC.}


}

\value{

A list of outputs, such as:

\item{T}{The score matrix (\eqn{n, nlv}).}

\item{P}{The loadings matrix (\eqn{p, nlv}).}

\item{R}{The projection matrix (= \eqn{P}) (\eqn{p, nlv}).}

\item{sv}{The singular values (\eqn{min(n, p), 1}) except for NIPALS = (\eqn{nlv, 1}).}

\item{eig}{The eigenvalues (\code{= sv^2}) (\eqn{min(n, p), 1}) except for NIPALS = (\eqn{nlv, 1}).}

\item{xmeans}{The centering vector of \eqn{X} (\eqn{p, 1}).}

\item{niter}{Numbers of iterations of the NIPALS.}

\item{conv}{Logical indicating if the NIPALS converged before reaching the maximal number of iterations.}

}

\references{

Andrecut, M., 2009. Parallel GPU Implementation of Iterative PCA Algorithms. Journal of Computational Biology 16, 1593-1599. https://doi.org/10.1089/cmb.2008.0221

Gabriel, R. K., 2002. Le biplot - Outil d'exploration de données multidimensionnelles. Journal de la Société Française de la Statistique, 143, 5-55.

Lingen, F.J., 2000. Efficient Gram–Schmidt orthonormalisation on parallel computers. Communications in Numerical Methods in Engineering 16, 57-66. https://doi.org/10.1002/(SICI)1099-0887(200001)16:1<57::AID-CNM320>3.0.CO;2-I

Tenenhaus, M., 1998. La régression PLS: théorie et pratique. Editions Technip, Paris, France.

Wright, K., 2018. Package nipals: Principal Components Analysis using NIPALS with Gram-Schmidt Orthogonalization. https://cran.r-project.org/

Wu, W., Massart, D.L., de Jong, S., 1997. The kernel PCA algorithms for wide data. Part I: Theory and algorithms. Chemometrics and Intelligent Laboratory Systems 36, 165-172. https://doi.org/10.1016/S0169-7439(97)00010-5

}

\examples{

n <- 6 ; p <- 4
Xtrain <- matrix(rnorm(n * p), nrow = n)
s <- c(3, 4, 7, 10, 11, 15, 21:24)   
zX <- replace(Xtrain, s, NA)
Xtrain
zX
m <- 2
Xtest <- matrix(rnorm(m * p), nrow = m)

pcasvd(Xtrain, nlv = 3)
pcaeigen(Xtrain, nlv = 3)
pcaeigenk(Xtrain, nlv = 3)
pcanipals(Xtrain, nlv = 3)
pcanipalsna(Xtrain, nlv = 3)
pcanipalsna(zX, nlv = 3)

fm <- pcaeigen(Xtrain, nlv = 3)
fm$T
transform(fm, Xtest)
transform(fm, Xtest, nlv = 2)

pcaeigen(Xtrain, nlv = 3)$T
pcaeigen(Xtrain, nlv = 3, weights = 1:n)$T

## Scores

Ttrain <- fm$T
Ttest <- transform(fm, Xtest)
T <- rbind(Ttrain, Ttest)
group <- c(rep("Training", nrow(Ttrain)), rep("Test", nrow(Ttest)))
i <- 1
plotxy(T[, i:(i+1)], group = group, pch = 16, zeroes = TRUE, cex = 1.3)

## Loadings

plotxy(fm$P, zeroes = TRUE, label = TRUE, cex = 2, col = "red3")

## Summary

summary(fm)
summary(fm, Xtrain)

res <- summary(fm, Xtrain)
plotxy(res$cor.circle, zeroes = TRUE, label = TRUE, cex = 2, col = "red3",
    circle = TRUE, ylim = c(-1, 1))

}

\keyword{datagen}