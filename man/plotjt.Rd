\name{plotjit}
\alias{plotjit}
\encoding{latin1}

\title{Jittered plot}

\description{Plot comparing classes with jittered points (random noise is added to the x-axis values for avoiding overplotting).}

\usage{
plotjit(x, y, group = NULL, 
  jit = 1, col = NULL, alpha.f = .8,
  legend = TRUE, legend.title = NULL, ncol = 1, med = TRUE,
  ...)
}

\arguments{

\item{x}{A vector of length \eqn{n} defining the class membership of the observations (x-axis).}

\item{y}{A vector of length \eqn{n} defining the variable to plot (y-axis).}

\item{group}{A vector of length \eqn{n} defining groups of observations to be plotted with different colors (default to \code{NULL}).}

\item{jit}{Scalar defining the jittering magnitude. Default to 1.}

\item{alpha.f}{Scalar modifying the opacity of the points in the graphics; typically in [0,1]. See \code{\link{adjustcolor}}.}

\item{col}{A color, or a vector of colors (of length equal to the number of classes or groups), defining the color(s) of the points.}

\item{legend}{Only if there are groups. Logical indicationg is a legend is drawn for groups (Default to \code{FALSE}).}

\item{legend.title}{Character string indicationg a title for the legend.}

\item{ncol}{Number of columns drawn in the legend box.}

\item{med}{Logical. If \code{TRUE} (default), the median of each class is plotted.}

\item{...}{Other arguments to pass in \code{\link{plot}}.}

}

\value{A plot.}



\examples{

n <- 500
x <- c(rep("A", n), rep("B", n))
y <- c(rnorm(n), rnorm(n, mean = 5, sd = 3))
group <- sample(1:2, size = 2 * n, replace = TRUE)

plotjit(x, y, pch = 16, jit = .5, alpha.f = .5)

plotjit(x, y, pch = 16, jit = .5, alpha.f = .5,
  group = group)


}

\keyword{datagen}