\name{odis}
\alias{odis}
\alias{lodis}
\encoding{latin1}

\title{Orthogonal distances from a PCA or PLS score space}

\description{

\code{odis} calculates the orthogonal distances (OD = "X-residuals") for a PCA or PLS model. OD is the Euclidean distance of a row observation to its projection to the score plan (see e.g. Hubert et al. 2005, Van Branden & Hubert 2005, p. 66; Varmuza & Filzmoser, 2009, p. 79).

A distance cutoff is computed using a moment estimation of the parameters of a Chi-squared distrbution for OD^2 (see Nomikos & MacGregor 1995, and Pomerantzev 2008). In the function output, column \code{dstand}  is a standardized distance defined as \eqn{OD / cutoff}. A value \code{dstand > 1} can be considered as extreme.

The cutoff for detecting extreme OD values is computed using a moment estimation of a Chi-squared distrbution for the squared distance.

}

\usage{

odis(
    object, Xtrain, X = NULL, 
    nlv = NULL,
    rob = TRUE, alpha = .01
    )
}

\arguments{

\item{object}{A fitted model, output of a call to a fitting function.}

\item{Xtrain}{Training X-data that was used to fit the model.}

\item{X}{New X-data.}

\item{nlv}{Number of components (PCs or LVs) to consider.}

\item{rob}{Logical. If \code{TRUE}, the moment estimation of the distance cutoff is robustified. This can be recommended after robust PCA or PLS on small data sets containing extreme values.}

\item{alpha}{Risk-\eqn{I} level for defining the cutoff detecting extreme values.}

}

\value{See examples.}

\references{

M. Hubert, P. J. Rousseeuw, K. Vanden Branden (2005). ROBPCA: a new approach to robust principal components analysis. Technometrics, 47, 64-79.

Nomikos, P., MacGregor, J.F., 1995. Multivariate SPC Charts for Monitoring Batch Processes. null 37, 41–59. https://doi.org/10.1080/00401706.1995.10485888

Pomerantsev, A.L., 2008. Acceptance areas for multivariate classification derived by projection methods. Journal of Chemometrics 22, 601–609. https://doi.org/10.1002/cem.1147

K. Vanden Branden, M. Hubert (2005). Robuts classification in high dimension based on the SIMCA method. Chem. Lab. Int. Syst, 79, 10-21. 

K. Varmuza, P. Filzmoser (2009). Introduction to multivariate statistical analysis in chemometrics. CRC Press, Boca Raton.

}

\examples{

n <- 6 ; p <- 4
Xtrain <- matrix(rnorm(n * p), ncol = p)
ytrain <- rnorm(n)
Xtest <- Xtrain[1:3, , drop = FALSE] 

nlv <- 3
fm <- pcasvd(Xtrain, nlv = nlv)
#fm <- plskern(Xtrain, ytrain, nlv = nlv)
odis(fm, Xtrain)
odis(fm, Xtrain, nlv = 2)
odis(fm, Xtrain, X = Xtest, nlv = 2)

}

\keyword{datagen}