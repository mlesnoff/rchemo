\name{aicplsr}
\alias{aicplsr}
\encoding{latin1}

\title{AIC and Cp for Univariate PLSR Models}

\description{

Computation of the AIC and Mallows's \eqn{Cp} criteria for univariate PLSR models (Lesnoff et al. 2021). This function may receive modifications in the future (work in progress).

For a model with \eqn{a} latent variables (LVs), function \code{aicplsr} calculates \eqn{AIC} and \eqn{Cp} by:

\eqn{AIC(a) = n * log(SSR(a)) + 2 * (df(a) + 1)}  

\eqn{Cp(a) = SSR(a) / n + 2 * df(a) * s2 / n}  

where \eqn{SSR} is the sum of squared residuals for the current evaluated model, \eqn{df(a)} the estimated PLSR model complexity (i.e. nb. model's degrees of freedom), \eqn{s2} an estimate of the irreductible error variance (computed from a low biased model) and \eqn{n} the number of training observations.  

By default (argument \code{correct}), the small sample size correction (so-called AICc) is applied to AIC and Cp for deucing the bias. 

The functions returns two estimates of Cp, each corresponding to a different estimate of s2.

The model complexity  \eqn{df} can be computed from three methods (argument \code{meth}).

}

\usage{

aicplsr(
    X, y, nlv, algo = NULL,
    meth = c("cg", "div", "cov"),
    correct = TRUE, B = 50, 
    print = FALSE, ...)

}

\arguments{

\item{X}{A \eqn{n x p} matrix or data frame of training observations.}

\item{y}{A vector of length \eqn{n} of training responses. }

\item{nlv}{The maximal number of latent variables (LVs) to consider in the model.}

\item{algo}{a PLS algorithm. Default to  \code{NULL} (\code{\link{plskern}} is used).}

\item{meth}{Method used for estimating \eqn{df}. Possible values are \code{"cg"} (\code{\link{dfplsr_cg}}), \code{"cov"} (\code{\link{dfplsr_cov}})or \code{"div"} (\code{\link{dfplsr_div}}).}

\item{correct}{Logical. If code{TRUE} (default), the AICc corection is applied to the criteria.}

\item{B}{For \code{meth = "dif"}: the number of observations in the data receiving perturbation (maximum is \eqn{n}; see \code{\link{dfplsr_cov}}). For \code{meth = "cov"}: the number of bootstrap replications (see \code{\link{dfplsr_cov}}).}

\item{print}{Logical. If \code{TRUE}, fitting information are printed.}

\item{...}{Optionnal arguments to pass in \code{algo}.}

}

\value{

A list of items, in particular a data.frame with the estimated criteria.
}

\references{

Burnham, K.P., Anderson, D.R., 2002. Model selection and multimodel inference: a practical informationtheoretic approach, 2nd ed. Springer, New York, NY, USA.

Burnham, K.P., Anderson, D.R., 2004. Multimodel Inference: Understanding AIC and BIC in Model
Selection. Sociological Methods & Research 33, 261–304. https://doi.org/10.1177/0049124104268644

Efron, B., 2004. The Estimation of Prediction Error. Journal of the American Statistical Association 99,
619–632. https://doi.org/10.1198/016214504000000692

Eubank, R.L., 1999. Nonparametric Regression and Spline Smoothing, 2nd ed, Statistics: Textbooks
and Monographs. Marcel Dekker, Inc., New York, USA.

Hastie, T., Tibshirani, R.J., 1990. Generalized Additive Models, Monographs on statistics and applied
probablity. Chapman and Hall/CRC, New York, USA.

Hastie, T., Tibshirani, R., Friedman, J., 2009. The elements of statistical learning: data mining,
inference, and prediction, 2nd ed. Springer, NewYork.

Hastie, T., Tibshirani, R., Wainwright, M., 2015. Statistical Learning with Sparsity: The Lasso and
Generalizations. CRC Press

Hurvich, C.M., Tsai, C.-L., 1989. Regression and Time Series Model Selection in Small Samples. Biometrika
76, 297. https://doi.org/10.2307/2336663

Lesnoff, M., Roger, J.M., Rutledge, D.N., Submitted. Monte Carlo methods for estimating Mallows’s Cp and AIC criteria for PLSR models. Illustration on agronomic spectroscopic NIR data. Journal of Chemometrics.

Mallows, C.L., 1973. Some Comments on Cp. Technometrics 15, 661–675.
https://doi.org/10.1080/00401706.1973.10489103

Ye, J., 1998. On Measuring and Correcting the Effects of Data Mining and Model Selection. Journal of
the American Statistical Association 93, 120–131. https://doi.org/10.1080/01621459.1998.10474094

Zuccaro, C., 1992. Mallows’ Cp Statistic and Model Selection in Multiple Linear Regression. International Journal of Market Research. 34, 1–10. https://doi.org/10.1177/147078539203400204

}

\examples{

data(cassav)

Xtrain <- cassav$Xtrain
ytrain <- cassav$ytrain

nlv <- 25
res <- aicplsr(Xtrain, ytrain, nlv = nlv)
#res <- aicplsr(Xtrain, ytrain, nlv = nlv, meth = "cov", B = 30)
names(res)
headm(res$crit)

z <- res$crit
par(mfrow = c(1, 4))
plot(z$df[-1])
plot(z$aic[-1], type = "b", main = "AIC")
plot(z$cp1[-1], type = "b", main = "Cp1")
plot(z$cp2[-1], type = "b", main = "Cp2")
par(mfrow = c(1, 1))

}

\keyword{datagen}