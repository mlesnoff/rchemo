\name{wdist}
\alias{wdist}
\encoding{latin1}

\title{Distance-based weights}

\description{
Calculation of weights from a vector of distances using a decreasing inverse exponential function.

Let \eqn{d} be a vector of distances. 


1- Preliminary weights are calculated by \eqn{w = exp(-d / (h * mad(d)))} , where \eqn{h} is a scalar > 0 (scale factor).  

2- The weights corresponding to distances higher than \eqn{median(d) + cri * mad(d)}, where \eqn{cri} is a scalar > 0, are set to zero. This step is used for removing outliers. 

3- Finally, the weights are "normalized" between 0 and 1 by \eqn{w = w / max(w)}.

}

\usage{wdist(d, h, cri = 4, squared = FALSE)}

\arguments{

\item{d}{A vector of distances.}

\item{h}{A scaling factor (positive scalar). Lower is \eqn{h}, sharper is the decreasing function. See the examples.}

\item{cri}{A positive scalar used for defining outliers in the distances vector.}

\item{squared}{Logical. If \code{TRUE}, distances \eqn{d} are replaced by the squared distances in the decreasing function, which corresponds to a Gaussian (RBF) kernel function. Default to \eqn{FALSE)}.}

}

\value{

A vector of weights.

}

\examples{

x1 <- sqrt(rchisq(n = 100, df = 10))
x2 <- sqrt(rchisq(n = 10, df = 40))
d <- c(x1, x2)
h <- 2 ; cri <- 3
w <- wdist(d, h = h, cri = cri)

oldpar <- par(mfrow = c(1, 1))
par(mfrow = c(2, 2))
plot(d)
hist(d, n = 50)
plot(w, ylim = c(0, 1)) ; abline(h = 1, lty = 2)
plot(d, w, ylim = c(0, 1)) ; abline(h = 1, lty = 2)
par(oldpar)

d <- seq(0, 15, by = .5)
h <- c(.5, 1, 1.5, 2.5, 5, 10, Inf)
for(i in 1:length(h)) {
  w <- wdist(d, h = h[i])
  z <- data.frame(d = d, w = w, h = rep(h[i], length(d)))
  if(i == 1) res <- z else res <- rbind(res, z)
  }
res$h <- as.factor(res$h)
headm(res)
plotxy(res[, c("d", "w")], asp = 0, group = res$h, pch = 16)


}

\keyword{datagen}