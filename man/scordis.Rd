\name{scordis}
\alias{scordis}
\encoding{latin1}

\title{Score distances (SD) in a PCA or PLS score space}

\description{

\code{scordis} calculates the score distances (SD) for a PCA or PLS model. SD is the Mahalanobis distance of the projection of a row observation on the score plan to the center of the score space.

A distance cutoff is computed using a moment estimation of the parameters of a Chi-squared distrbution for SD^2 (see e.g. Pomerantzev 2008). In the function output, column \code{dstand}  is a standardized distance defined as \eqn{SD / cutoff}. A value \code{dstand > 1} can be considered as extreme.

The Winisi "GH" is also provided (usually considered as extreme if GH > 3).

}

\usage{

scordis(
    object, X = NULL, 
    nlv = NULL,
    rob = TRUE, alpha = .01
    )

}

\arguments{

\item{object}{A fitted model, output of a call to a fitting function.}

\item{X}{New X-data.}

\item{nlv}{Number of components (PCs or LVs) to consider.}

\item{rob}{Logical. If \code{TRUE}, the moment estimation of the distance cutoff is robustified. This can be recommended after robust PCA or PLS on small data sets containing extreme values.}

\item{alpha}{Risk-\eqn{I} level for defining the cutoff detecting extreme values.}

}

\value{See examples.}

\references{

M. Hubert, P. J. Rousseeuw, K. Vanden Branden (2005). ROBPCA: a new approach to robust principal components analysis. Technometrics, 47, 64-79.

Pomerantsev, A.L., 2008. Acceptance areas for multivariate classification derived by projection methods. Journal of Chemometrics 22, 601–609. https://doi.org/10.1002/cem.1147

}


\examples{

n <- 6 ; p <- 4
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- rnorm(n)
Xtest <- Xtrain[1:3, , drop = FALSE] 

nlv <- 3
fm <- pcasvd(Xtrain, nlv = nlv)
#fm <- plskern(Xtrain, ytrain, nlv = nlv)
scordis(fm)
scordis(fm, nlv = 2)
scordis(fm, Xtest, nlv = 2)

}

\keyword{datagen}