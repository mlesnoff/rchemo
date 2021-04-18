\name{plskern}
\alias{plskern}
\alias{plsnipals}
\alias{plsrannar}
\alias{transform.Pls}
\alias{summary.Pls}
\alias{coef.Pls}
\alias{predict.Plsr}
\encoding{latin1}

\title{PLS algorithms}

\description{

Algorithms fitting a (centered) PLS1 or PLS2 model between dependent variables \eqn{X} and responses \eqn{Y}.

- \code{plskern}: "Improved kernel algorithm #1" proposed by Dayal and MacGregor (1997). This algorithm is stable and fast (Andersson 2009), and returns the same results as the NIPALS. 

- \code{plsnipals}: NIPALS algorithm (e.g. Tenenhaus 1998, Wold 2002). In the function, the usual PLS2 NIPALS iterative is replaced by a direct calculation of the weights vector \eqn{w} by SVD decomposition of matrix \eqn{X'Y} (Hoskuldsson 1988 p.213).  

- \code{plsrannar}: Kernel algorithm proposed by Rannar et al. (1994) for "wide" matrices, i.e.  with low number of rows and very large number of columns (p >> n; e.g. p = 20000). In such a situation, this algorithm is faster than the others (but it becomes much slower in other situations). If the algorithm converges, it returns the same results as the NIPALS (Note: discrepancies can be observed if too many PLS components are requested compared to the low number of observations).

\bold{Auxiliary functions}

\code{transform} Calculates the LVs for any new matrix \eqn{X} from the model.

\code{summary} returns summary information for the model.

\code{coef} Calculates b-coefficients from the model.

\code{predict} Calculates the predictions for any new matrix \eqn{X} from the model.

}

\usage{

plskern(X, Y, nlv, weights = NULL)

plsnipals(X, Y, nlv, weights = NULL)

plsrannar(X, Y, nlv, weights = NULL)

\method{transform}{Pls}(object, X, ..., nlv = NULL)  

\method{summary}{Pls}(object, ...)  

\method{coef}{Pls}(object, ..., nlv = NULL) 

\method{predict}{Plsr}(object, X, ..., nlv = NULL)  

}

\arguments{

\item{X}{For the main functions: Training X-data (\eqn{n, p}). --- For auxiliary functions: New X-data (\eqn{m, p}) to consider.}

\item{Y}{Training Y-data (\eqn{n, q}).}

\item{nlv}{The number(s) of LVs to calculate.}

\item{weights}{Weights (\eqn{n, 1}) to apply to the training observations. Internally, weights are "normalized" to sum to 1. Default to \code{NULL} (weights are set to \eqn{1 / n}).}

\item{object}{A fitted model, output of a call to the main functions.}

\item{...}{Optional arguments.}

}

\value{
A list of outputs, such as:

\item{T}{The X-score matrix (\eqn{n, nlv}).}

\item{P}{The X-loadings matrix (\eqn{p, nlv}).}

\item{W}{The X-loading weights matrix (\eqn{p, nlv}).}

\item{C}{The Y-loading weights matrix (C = t(Beta), where Beta is the scores regression coefficients matrix).}

\item{R}{The PLS projection matrix (\eqn{p, nlv}).}

\item{xmeans}{The centering vector of \eqn{X} (\eqn{p, 1}).}

\item{ymeans}{The centering vector of \eqn{Y} (\eqn{q, 1}).}

}

\references{

Andersson, M., 2009. A comparison of nine PLS1 algorithms. Journal of Chemometrics 23, 518-529.

Dayal, B.S., MacGregor, J.F., 1997. Improved PLS algorithms. Journal of Chemometrics 11, 73-85.

Hoskuldsson, A., 1988. PLS regression methods. Journal of Chemometrics 2, 211-228. https://doi.org/10.1002/cem.1180020306

Kim, S., Kano, M., Nakagawa, H., Hasebe, S., 2011. Estimation of active pharmaceutical ingredients content using locally weighted partial least squares and statistical wavelength selection. Int. J. Pharm., 421, 269-274.

Lesnoff, M., Metz, M., Roger, J.M., 2020. Comparison of locally weighted PLS strategies for regression and discrimination on agronomic NIR Data. Journal of Chemometrics. e3209. https://onlinelibrary.wiley.com/doi/abs/10.1002/cem.3209

Rannar, S., Lindgren, F., Geladi, P., Wold, S., 1994. A PLS kernel algorithm for data sets with many variables and fewer objects. Part 1: Theory and algorithm. Journal of Chemometrics 8, 111-125. https://doi.org/10.1002/cem.1180080204

Schaal, S., Atkeson, C., Vijayamakumar, S. 2002. Scalable techniques from nonparametric statistics for the real time robot learning. Applied Intell., 17, 49-60.

Sicard, E. Sabatier, R., 2006. Theoretical framework for local PLS1 regression and application to a rainfall data set. Comput. Stat. Data Anal., 51, 1393-1410.

Tenenhaus, M., 1998. La régression PLS: théorie et pratique. Editions Technip, Paris, France.

Wold, S., Sjostrom, M., Eriksson, l., 2001. PLS-regression: a basic tool for chemometrics. Chem. Int. Lab. Syst., 58, 109-130.

}

\examples{

n <- 8 ; p <- 6
X <- matrix(rnorm(n * p), ncol = p)
y1 <- rnorm(n)
y2 <- rnorm(n)
Y <- cbind(y1, y2)

plskern(X, y1, nlv = 3)
plskern(X, Y, nlv = 3)
plskern(X, Y, nlv = 3)$T
plskern(X, Y, nlv = 3, weights = 1:n)$T

fm <- plskern(X, Y, nlv = 3)
coef(fm)
coef(fm, nlv = 0)
coef(fm, nlv = 1)

fm$T
transform(fm, X[1:2, ])
transform(fm, X[1:2, ], nlv = 1)

summary(fm)

predict(fm, X)
predict(fm, X, nlv = 0:3)

pred <- predict(fm, X)$pred
msep(pred, Y)

nlv <- 5
pars <- data.frame(nlv = 0:nlv)
gridscorelv(
    X, Y, X[1:2, ], Y[1:2, ], 
    score = msep, fun = plskern, 
    pars
    )

}

\keyword{datagen}