\name{plotxy}
\alias{plotxy}
\encoding{latin1}

\title{2-d scatter plot}

\description{2-dimension scatter plot.}

\usage{
plotxy(X, group = NULL, 
    asp = 0, col = NULL, alpha.f = .8,
    zeroes = FALSE, circle = FALSE, ellipse = FALSE,
    labels = FALSE,
    legend = TRUE, legend.title = NULL, ncol = 1,
    ...)
}

\arguments{

\item{X}{Data (\eqn{n, p}) to plot. If \eqn{p > 2}, only the first two columns are considered.}

\item{group}{Groups of observations (\eqn{n}) to be plotted with different colors (default to \code{NULL}).}

\item{asp}{Scalar. Giving the aspect ratio y/x. The value \code{asp = 0} is the default in \code{\link{plot.default}} (no constraints on the ratio). See \code{\link{plot.default}}.}

\item{col}{A color, or a vector of colors (of length equal to the number of groups), defining the color(s) of the groups.}

\item{alpha.f}{Scalar modifying the opacity of the points in the graphics; typically in [0,1]. See \code{\link{adjustcolor}}.}

\item{zeroes}{Logical indicationg if an horizontal and vertical lines are drawn at coordonates (0, 0) (Default to \code{FALSE}).}

\item{circle}{Not still working. Logical indicating if a correlation circle is plotted  (default to \code{FALSE}).}

\item{ellipse}{Logical indicating if a Gaussian  ellipse is plotted (default to \code{FALSE}). If there are groups, an ellipse is drawn for each group.}

\item{labels}{Logical indicating if the row names of \code{X} (instead of points) are plotted (default to \code{FALSE}).}

\item{legend}{Only if there are groups. Logical indicationg is a legend is drawn for groups (Default to \code{FALSE}).}

\item{legend.title}{Character string indicationg a title for the legend.}

\item{ncol}{Number of columns drawn in the legend box.}

\item{...}{Other arguments to pass in functions \code{\link{plot}}, \code{\link{points}}, \code{\link{axis}} and \code{\link{text}}.}

}

\value{A plot.}

\examples{

n <- 50 ; p <- 10
Xtrain <- matrix(rnorm(n * p), ncol = p)
Xtest <- Xtrain[1:5, ] + .4 

fm <- pcaeigen(Xtrain, nlv = 5)
Ttrain <- fm$T
Ttest <- transform(fm, Xtest)
T <- rbind(Ttrain, Ttest)
group <- c(rep("Training", nrow(Ttrain)), rep("Test", nrow(Ttest)))
i <- 1
plotxy(T[, i:(i+1)], group = group, 
       pch = 16, zeroes = TRUE,
       main = "PCA")
       
plotxy(T[, i:(i+1)], group = group, 
       pch = 16, zeroes = TRUE, asp = 1,
       main = "PCA")

}

\keyword{datagen}