\name{xfit}
\alias{xfit}
\alias{xssr}
\encoding{latin1}

\title{Matrix fitting from scores and loadings matrices and SSR calculation}

\description{

Function \code{xfit} calculates the estimate of a matrix \eqn{X} (say \eqn{Xfit}) from given multivariate (e.g. PCA or PLS) scores and loadings of dimension \eqn{a}.

Function \code{xssr} calculates the sum of the squared \eqn{X-}residuals \eqn{SSR[i] = sum(E[i]^2) (i = 0, ..., a)}, where \eqn{E[i] = X - Xfit[i]}, and \eqn{X[0]} the centered matrix \eqn{X}, The returned SSR is a vector of length \eqn{a + 1}.

}

\usage{

xfit(T, P, xmeans = rep(0, dim(P)[1]))

xssr(X, T, P, xmeans = rep(0, dim(P)[1]))

}

\arguments{

\item{X}{Only for \code{xssr}. The \eqn{n x p} matrix or data frame \eqn{X} for which SSR are calculated.}

\item{T}{A \eqn{n x a} scores matrix, or data.frame, calculated from a \eqn{n x p} matrix \eqn{X}.}

\item{P}{A \eqn{p x a} loadings matrix, or data.frame, that will be applied to \eqn{T}.}

\item{xmeans}{An eventual centering vector (length \eqn{p}) used for \eqn{X} before calculating \eqn{T}.}

}

\value{

\item{For \code{xfit}:}{Fitted matrix \eqn{X}.}

\item{For \code{xssr}:}{SSR vector of length \eqn{a + 1} (\eqn{SSR[i]  i = 0, ..., a}).}


}

\examples{

data(datcass)

Xr <- datcass$Xr
Xu <- datcass$Xu

headm(Xr)
headm(Xu)

ncomp <- 2
#ncomp <- 10
fm <- pca(Xr, Xu, ncomp = ncomp)

##################### Approximation of Xr

Xr.fit <- xfit(fm$Tr, fm$P, fm$xmeans)
headm(Xr.fit)

s <- sample(1:nrow(Xr), size = 1)
plotsp(Xr[s, , drop = FALSE], col = "red", zeroes = TRUE)
plotsp(Xr.fit[s, , drop = FALSE], col = "blue", add = TRUE, lty = 2)

##################### Approximation of Xu

Xu.fit <- xfit(fm$Tu, fm$P, fm$xmeans)
headm(Xu.fit)

s <- sample(1:nrow(Xu), size = 1)
plotsp(Xu[s, , drop = FALSE], col = "red", zeroes = TRUE)
plotsp(Xu.fit[s, , drop = FALSE], col = "blue", add = TRUE, lty = 2)

##################### SSR

xssr(Xr, fm$Tr, fm$P, fm$xmeans)

xssr(Xu, fm$Tu, fm$P, fm$xmeans)

}

\keyword{datagen}