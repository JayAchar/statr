% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round_percent.R
\name{round_percent}
\alias{round_percent}
\title{Round vector of number to percentages}
\usage{
round_percent(x, decimals = 0L, ties = c("random", "last"))
}
\arguments{
\item{x}{A numeric vector with non-negative values.}

\item{decimals}{An integer giving the number of decimals that are used}

\item{ties}{A string that is either 'random' (the default) or 'last'. 
Determines how to break ties. Random is random, last prefers to break ties at the last position}
}
\value{
Returns a vector of numeric values of the same length as x.
}
\description{
Round vector of number to percentages
}
\examples{
round_percent(x = c(10, 10, 20), 0L)
}
\seealso{
\code{\link{statr}}
}
\author{
Jay Achar
}
