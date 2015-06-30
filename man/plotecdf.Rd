\name{plotecdf}
\alias{plotecdf}
\alias{plotprcc}
\title{
	Uncertainty and sensitivity plots
}
\description{
	Plots the empirical cumulative density function
	or the partial rank correlation coefficient from an LHS object
	or PLUE object. 
}
\usage{
plotecdf(obj, stack = FALSE, index.res, col, xlab = NULL, ...)
plotprcc(obj, index.res, col, ylab = NULL, ...)
}
\arguments{
  \item{obj}{
	  The LHS or PLUE object containing the simulation results to be plotted.

	  NOTICE: plotecdf only accepts LHS objects! For plotting the likelihood profile
	  from a PLUE object, simply use \code{plot(obj)}
}
  \item{stack}{
	  If the results is a data.frame with several variables, stack=FALSE generates
	  a series of plots, and stack=TRUE generates a single plot with the ECDF from
	  all variables identified by different colors.
}
  \item{index.res}{
	  An optional vector indicating which columns from the results are to be plotted.
}
  \item{col}{
	  An optional vector indicating the colors to be used.
}
  \item{xlab, ylab}{
	  Label for the x axis (ecdf) or y axis(prcc). 
	  Uses the name provided in the res.names argument from the LHS function if left blank.
  }
  \item{\dots}{
	  Additional parameters to be passed to the lower level plotting function.
}
}
\examples{
myLHS <- LHS(model=function(x) x[,1]+x[,2]*x[,3], factors=3, N=20, res.names="My Output")
plotecdf(myLHS, main="ECDF plot")
plotprcc(myLHS, main="PRCC plot")
}
