\name{octane}
\alias{octane}
\docType{data}
\encoding{latin1}

\title{octane}

\description{

Octane dataset.

Near infrared (NIR) spectra (absorbance) of \eqn{n} = 39 gasoline samples over \eqn{p} = 226 wavelengths (1102 nm to 1552 nm, step = 2 nm).

Samples 25, 26, and 36-39 contain added alcohol (outliers).

}

\usage{data(octane)}

\format{
  A list with 1 component: matrix \code{X}.
}

\source{

K.H. Esbensen, S. Schoenkopf and T. Midtgaard Multivariate Analysis in Practice, Trondheim, Norway: Camo, 1994.

Todorov, V. 2020. rrcov: Robust Location and Scatter Estimation and Robust Multivariate Analysis with High Breakdown. R Package version 1.5-5. https://cran.r-project.org/.

}

\references{

M. Hubert, P. J. Rousseeuw, K. Vanden Branden (2005), ROBPCA: a new approach to robust principal components analysis, Technometrics, 47, 64–79.

P. J. Rousseeuw, M. Debruyne, S. Engelen and M. Hubert (2006), Robustness and Outlier Detection in Chemometrics, Critical Reviews in Analytical Chemistry, 36(3–4), 221–242.

}

\examples{

data(octane)

X <- octane$X
headm(X)

plotsp(X, xlab = "Wawelength", ylab = "Absorbance")
plotsp(X[c(25:26, 36:39), ], add = TRUE, col = "red")

}


\keyword{datasets}
