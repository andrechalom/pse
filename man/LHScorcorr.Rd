\name{LHScorcorr}
\alias{LHScorcorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Corrects the correlation matrix of a given Latin Hypercube Sample
%%  ~~function to do ... ~~
}
\description{
	This function changes the order in which data is organized in order
	to force the correlation matrix to a prescribed value. This implementation
	uses the Hungtington-Lyrintzis algorithm.

	Intended for use inside of the LHS function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LHScorcorr(vars, COR=0, eps = 0.005, echo=FALSE, maxIt=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vars}{
	  The data.frame containing the parameters from the "raw" Latin Hypercube Sample.
	  Each column corresponds to one variable, and each line to one observation.
%%     ~~Describe \code{vars} here~~
}
  \item{COR}{
	  The desired correlation matrix. The default is to have 0 correlation.

	  You can supply a numeric square matrix with M rows, where M is the number of
	  input factors. The *lower* triangular part of the matrix will be used as the 
	  desired correlation matrix.
}
  \item{eps}{
	  The tolerance for the deviation between the prescribed correlation matrix and the result.
}
  \item{echo}{
	  Set to true to display information messages.
}
  \item{maxIt}{
	  Maximum number of iterations before giving up. 
	  Set to 0 to use a heuristic based on the size of the hypercube.
	  Set to a negative number to never give up. *CAUTION*, this might result in an infinite loop.
}
}
\value{
	A data.frame containing the same variables, but with the correlation matrix corrected.
}
\references{
Huntington, D.E.  and  Lyrintzis, C.S.; Improvements to and limitations of Latin hypercube sampling. Prob. Engng. Mech. 1998 13 (4): 245-253.
}
