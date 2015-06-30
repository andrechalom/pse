\name{plotscatter}
\alias{plotscatter}
\title{Produces a series of scatterplots from data
}
\description{
	Produces a series of scatterplots from data.
}
\usage{
plotscatter(vars, res = NULL, index.data, index.res, add.lm, ylab = NULL, ...)
}
\arguments{
  \item{vars}{
	  Either a data.frame consisting of the data to plot on the x axis, or an LHS object,
	  or a PLUE object containing the data to be plot
}
  \item{res}{
	  A data.frame consisting of the model results to be plotted on the y axis, if
	  vars is passed as a data.frame.
	  If vars is an LHS object, this parameter is ignored.
}
  \item{index.data}{
	  The indices of the data columns to be plotted. Useful when plotting from LHS objects
}
  \item{index.res}{
	  The indices of the result columns to be plotted. Useful when plotting from LHS objects
}
  \item{add.lm}{
	  Boolean. Whether to include a simple linear model on the plots. Defaults to TRUE.
}
  \item{ylab}{
	  Label for the x axis. 
	  Uses the name provided in the res.names argument from the LHS function if left blank.
  }
  \item{\dots}{
	  Additional parameters to pass to the lower level plotting functions
}
}
\note{
	This function was called \code{corPlot} in older releases, 
	renamed to \code{plotscatter} for padronization with other graphical
	functions. The function \code{corPlot} was removed in pse 0.4.0.
}
\examples{
myLHS <- LHS(model=function(x) x[,1]+x[,2]*x[,3], factors=3, N=20)
plotscatter(myLHS)
}
