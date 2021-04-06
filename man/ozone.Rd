\name{ozone}
\alias{ozone}
\docType{data}
\encoding{latin1}

\title{ozone}

\description{

Los Angeles ozone pollution data in 1976
(sources: Breiman & Friedman 1985,  Leisch &  Dimitriadou 2020).

366 observations, 13 variables:

- 1:	Month: 1 = January, ..., 12 = December

- 2:	Day of month

- 3:	Day of week: 1 = Monday, ..., 7 = Sunday

- 4:	Daily maximum one-hour-average ozone reading

- 5:	500 millibar pressure height (m) measured at Vandenberg AFB

- 6:	Wind speed (mph) at Los Angeles International Airport (LAX)

- 7:	Humidity (%) at LAX

- 8:	Temperature (degrees F) measured at Sandburg, CA

- 9:  Temperature (degrees F) measured at El Monte, CA

- 10:	Inversion base height (feet) at LAX

- 11:	Pressure gradient (mm Hg) from LAX to Daggett, CA

- 12:	Inversion base temperature (degrees F) at LAX

- 13:	Visibility (miles) measured at LAX

The variable to predict is V4.

}

\usage{data(ozone)}

\format{
  A list with 1 component: matrix \code{X}.
}

\source{

Breiman L., Friedman J.H. 1985. Estimating optimal transformations for multiple regression and correlation, JASA, 80, pp. 580-598.

Leisch, F. and Dimitriadou, E. (2010). mlbench: Machine Learning Benchmark Problems.
R package version 1.1-6. https://cran.r-project.org

}

\examples{

data(ozone)

z <- ozone$X
head(z)

plotxna(z)

}


\keyword{datasets}
