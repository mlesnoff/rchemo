\name{mse}
\alias{mse}
\alias{msep}
\alias{rmsep}
\alias{sep}
\alias{bias}
\alias{r2}
\alias{cor2}
\alias{err}
\alias{residreg}
\alias{residcla}
\encoding{latin1}

\title{Residuals and prediction error rates}

\description{

Residuals and prediction error rates (MSEP, SEP, etc. or classification error rate)  for models with quantitative or qualitative responses. 

Note: The rate \eqn{R2} is calculated by \eqn{R2 = 1 - MSEP(fitted model) / MSEP(no model)}, where  \eqn{MSEP = Sum((y_i - fit_i)^2)/n} and "no model" is the overall mean of \eqn{y}. For predictions over CV or Test sets, and/or for non linear models, it can be different from the square of the correlation coefficient (\eqn{cor2}) between the observed values and prediction. 

}

\usage{

residreg(pred, Y)
residcla(pred, y)

msep(pred, Y)
rmsep(pred, Y)
sep(pred, Y)
bias(pred, Y)
r2(pred, Y)
cor2(pred, Y)

mse(pred, y, digits = 3)

err(pred, y)

}

\arguments{

\item{pred}{Prediction (\eqn{m, q}); output of a function \code{predict}.}

\item{Y}{Observed response (\eqn{m, q}).}

\item{y}{Observed response (\eqn{m, 1}).}

\item{digits}{Number of digits for the numerical outputs.}

}

\value{

Residuals, or error rates.

}

\keyword{datagen}