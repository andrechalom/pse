\name{target.sbma}
\alias{target.sbma}
\title{
	Adaptative generation of Latin Hypercubes
}
\description{
	Generates a series of Latin Hypercube Samples for a model until a pair of LHS
	present a measure of agreement equal to or greater than a specified target.
}
\usage{
target.sbma(target, model, factors, q = NULL, q.arg = NULL, res.names = NULL, 
			method=c("HL", "random"), opts=list(), init, inc, FUN = min)
}
\arguments{
  \item{target}{
	  The desired SBMA.
}
  \item{model}{
	  The function to be run, representing the model or simulation.
	  If NULL, no function is run and the object generated is incomplete, see also the 'tell' method.
}
  \item{factors}{
	  The names of the input variables (used for naming the 'data' data.frame and in plotting)
}
  \item{q}{
	The quantile functions to be used. If only one is provided, it will be used for all parameters.
}
  \item{q.arg}{
	  A list containing the arguments for the 'q' functions. Each parameter must be specified by a named
	  list, containing all of the arguments for the quantile distribution.
}
  \item{res.names}{
	  Optional: what are the names of the model results? (Used mainly for plotting)
}
  \item{method}{
	  The desired method for the generation of the hypercubes. Please refer to the \code{LHS} help page.
}
  \item{opts}{
	  Additional options for the method chosen. Please refer to the \code{LHS} help page.
}
  \item{init}{
	  The size of the initial LHS generated.
}
  \item{inc}{
	  The increment between successive runs. For example, if init = 5 and inc = 20, the first
	  LHS will be generated with size 5, the second with size 25.
}
  \item{FUN}{
	  When the model returns more than one response, SBMA values are calculated for each variable.
	  The FUN argument specifies how to combine these SBMA values. The recommended default is to
	  chose the minimum value.
}
}
\value{
	Returns the largest LHS generated.
}
