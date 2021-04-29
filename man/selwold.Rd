\name{selwold}
\alias{selwold}
\encoding{latin1}

\title{Heuristic selection of the dimension of a latent variable model with the Wold's criterion}

\description{

The function helps selecting the dimensionnality of latent variable (LV) models (e.g. PLSR) using the "Wold criterion". 

The criterion is the "precision gain ratio" \eqn{R = 1 - r(a+1) / r(a)} where \eqn{r} is an observed error rate quantifying the model performance (msep, classification error rate, etc.) and \eqn{a} the model dimensionnality (= nb. LVs). It can also represent other indicators such as the eigenvalues of a PCA.

\eqn{R} is the relative gain in efficiency after a new LV is added to the model. The iterations continue until \eqn{R} becomes lower than a threshold value \eqn{alpha}. By default and only as an indication, the default \eqn{alpha = .05} is set in the function, but the user should set any other value depending on his data and parcimony objective.

In the original article, Wold (1978; see also Bro et al. 2008) used the ratio of \bold{cross-validated} over \bold{training} residual sums of squares, i.e. PRESS over SSR. Instead, \code{selwold} compares values of consistent nature (the successive values in the input vector \eqn{r}), e.g. PRESS only . For instance, \eqn{r} was set to PRESS values in Li et al. (2002) and Andries et al. (2011), which is equivalent to the "punish factor" described in Westad & Martens (2000).

The ratio \eqn{R} is often erratic, making difficult the dimensionnaly selection. Function \code{selwold} proposes to calculate a smoothing of \eqn{R} (argument \eqn{smooth}).


}

\usage{
selwold(
    r, indx = seq(length(r)), 
    smooth = TRUE, f = 1/3,
    alpha = .05, digits = 3,
    plot = TRUE,
    xlab = "Index", ylab = "Value", main = "r",
    ...
    )
  }

\arguments{

\item{r}{Vector of a given error rate (\eqn{n}) or any other indicator.}

\item{indx}{Vector of indexes (\eqn{n}), typically the nb. of Lvs.}

\item{smooth}{Logical. If \code{TRUE} (default), the selection is done on the smoothed \eqn{R}.}

\item{f}{Window for smoothing \eqn{R} with function \code{\link{lowess}}.}

\item{alpha}{Proportion \eqn{alpha} used as threshold for \eqn{R}.}

\item{digits}{Number of digits for \eqn{R}.}

\item{plot}{Logical. If \code{TRUE} (default), results are plotted.}

\item{xlab}{x-axis label of the plot of \eqn{r} (left-side in the graphic window).}

\item{ylab}{y-axis label of the plot of \eqn{r} (left-side in the graphic window).}

\item{main}{Title of the plot of \eqn{r} (left-side in the graphic window).}

\item{...}{Other arguments to pass in function \code{\link{lowess}}.}

}

\value{

A list of outputs (see examples), such as:

\item{opt}{The index of the minimum for \eqn{r}.}

\item{sel}{The index of the selection from the \eqn{R} (or smoothed \eqn{R}) threshold.}

}


\references{

Andries, J.P.M., Vander Heyden, Y., Buydens, L.M.C., 2011. Improved variable reduction in partial least squares modelling based on Predictive-Property-Ranked Variables and adaptation of partial least squares complexity. Analytica Chimica Acta 705, 292-305. https://doi.org/10.1016/j.aca.2011.06.037

Bro, R., Kjeldahl, K., Smilde, A.K., Kiers, H.A.L., 2008. Cross-validation of component models: A critical look at current methods. Anal Bioanal Chem 390, 1241-1251. https://doi.org/10.1007/s00216-007-1790-1

Li, B., Morris, J., Martin, E.B., 2002. Model selection for partial least squares regression. Chemometrics and Intelligent Laboratory Systems 64, 79-89. https://doi.org/10.1016/S0169-7439(02)00051-5

Westad, F., Martens, H., 2000. Variable Selection in near Infrared Spectroscopy Based on Significance Testing in Partial Least Squares Regression. J. Near Infrared Spectrosc., JNIRS 8, 117–124.

Wold S. Cross-Validatory Estimation of the Number of Components in Factor and Principal Components Models. Technometrics. 1978;20(4):397-405

}

\examples{

data(cassav)

Xtrain <- cassav$Xtrain
ytrain <- cassav$ytrain
X <- cassav$Xtest
y <- cassav$ytest

nlv <- 20
res <- gridscorelv(
    Xtrain, ytrain, X, y, 
    score = msep, fun = plskern, 
    nlv = 0:nlv
    )
selwold(res$y1, res$nlv, f = 2/3)

}





\keyword{datagen}