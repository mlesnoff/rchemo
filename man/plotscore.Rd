\name{plotscore}
\alias{plotscore}
\encoding{latin1}

\title{Plotting errors rates}

\description{Plotting scores of prediction errors (error rates).}

\usage{
plotscore(x, y, group = NULL, 
    col = NULL, steplab = 2, legend = TRUE, legend.title = NULL, ncol = 1, ...)
}

\arguments{

\item{x}{Horizontal axis vector (\eqn{n}).}

\item{y}{Vertical axis vector (\eqn{n})}

\item{group}{Groups of data (\eqn{n}) to be plotted with different colors.}

\item{col}{A color, or a vector of colors (of length equal to the number of groups), defining the color(s) of the groups.}

\item{steplab}{A step for the horizontal axis. Can be \code{NULL} (automatic step).}

\item{legend}{Only if there are groups. Logical indicationg is a legend is drawn for groups (Default to \code{FALSE}).}

\item{legend.title}{Character string indicationg a title for the legend.}

\item{ncol}{Number of columns drawn in the legend box.}

\item{...}{Other arguments to pass in function \code{\link{plot}}.}

}

\value{A plot.}

\examples{

n <- 50 ; p <- 20
Xtrain <- matrix(rnorm(n * p), ncol = p, byrow = TRUE)
ytrain <- rnorm(n)
Ytrain <- cbind(ytrain, 10 * rnorm(n))
m <- 3
Xtest <- Xtrain[1:m, ] 
Ytest <- Ytrain[1:m, ] ; ytest <- Ytest[, 1]

nlv <- 15
res <- gridscorelv(
    Xtrain, ytrain, Xtest, ytest, 
    score = msep, 
    fun = plskern, 
    nlv = 0:nlv, verb = TRUE
    )
plotscore(res$nlv, res$y1, 
          main = "MSEP", xlab = "Nb. LVs", ylab = "Value")

nlvdis <- 5
h <- c(1, Inf)
k <- c(10, 20)
nlv <- 15
pars <- mpars(nlvdis = nlvdis, diss = "mahal",
              h = h, k = k)
res <- gridscorelv(
    Xtrain, Ytrain, Xtest, Ytest, 
    score = msep, 
    fun = lwplsr, 
    nlv = 0:nlv, pars = pars, verb = TRUE
)
headm(res)
group <- paste("h=", res$h, " k=", res$k, sep = "")
plotscore(res$nlv, res$y1, group = group,
          main = "MSEP", xlab = "Nb. LVs", ylab = "Value")

}

\keyword{datagen}