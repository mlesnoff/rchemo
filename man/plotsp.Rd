\name{plotsp}
\alias{plotsp}
\alias{plotsp1}
\encoding{latin1}

\title{Plotting spectra}

\description{

\code{plotsp} plots lines corresponding to the row observations (e.g. spectra) of a data set.

\code{plotsp1} plots only one observation per plot (e.g. spectrum by spectrum) by scrolling the rows. After running a \code{plotsp1} command, the plots are printed successively by pushing the R console "entry button", and stopped by entering any character in the R console.

}

\usage{

plotsp(X,
  type = "l", col = NULL, zeroes = FALSE, labels = FALSE, 
  add = FALSE,
  ...)

plotsp1(X, col = NULL, zeroes = FALSE, ...)

}
  
\arguments{

\item{X}{Data (\eqn{n, p}) to plot.}

\item{type}{1-character string giving the type of plot desired. Default value to \code{"l"} (lines). See \code{\link{plot.default}} for other options.}

\item{col}{A color, or a vector of colors (of length n), defining the color(s) of the lines representing the rows.}

\item{zeroes}{Logical indicationg if an horizontal line is drawn at coordonates (0, 0) (Default to \code{FALSE}).}

\item{labels}{Logical indicating if the row names of \code{X} are plotted (default to \code{FALSE}).}

\item{add}{Logical defining if the frame of the plot is plotted (\code{add = FALSE}; default) or not (\code{add = TRUE}). This allows to add new observations to a plot without red-building the frame.}

\item{...}{Other arguments to pass in functions \code{\link{plot}} or \code{\link{lines}}}.

}

\value{

A plot (see examples).

}

\examples{

data(cassav)

X <- cassav$Xtest
n <- nrow(X)

plotsp(X)
plotsp(X, col = "grey")
plotsp(X, col = "lightblue", 
  xlim = c(500, 1500),
  xlab = "Wawelength (nm)", ylab = "Absorbance")

### See: ?hcl.colors, hcl.pals()
col <- hcl.colors(n = n, alpha = 1, rev = FALSE, palette = "Grays")
#col <- hcl.colors(n = n, alpha = 1, rev = FALSE, palette = "Green-Orange")
#col <- terrain.colors(n, rev = FALSE)
#col <- rainbow(n, rev = FALSE, alpha = .2)
plotsp(X, col = col)

plotsp(X, col = "grey")
plotsp(X[23, , drop = FALSE], lwd = 2, add = TRUE)
plotsp(X[c(23, 16), ], lwd = 2, add = TRUE)

plotsp(X[5, , drop = FALSE], labels = TRUE)

plotsp(X[c(5, 61), ], labels = TRUE)

col <- hcl.colors(n = n, alpha = 1, rev = FALSE, palette = "Grays")
plotsp(X, col = col)
plotsp(X[5, , drop = FALSE], col = "red", lwd = 2, add = TRUE, labels = TRUE)

###### Example with plotsp1
###### Scrolling plot of PCA loadings 

## After running the code, 
## type Enter in the R console for starting the scrolling,
## and type any character in the R console 

fm <- pcaeigenk(X, nlv = 20)
P <- fm$P
## Run the plot:
#plotsp1(t(P), ylab = "Value")

}

\keyword{datagen}