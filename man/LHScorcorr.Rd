\name{LHScorcorr}
\alias{LHScorcorr}
\title{
	Corrects the correlation matrix of a given Latin Hypercube Sample
}
\description{
	This function changes the order in which data is organized in order
	to force the correlation matrix to a prescribed value. This implementation
	uses the Hungtington-Lyrintzis algorithm.

	Intended for use inside of the \code{\link{LHS}} function.

  If you intend to use non-zero correlation terms, read Chalom & Prado (2012) for some
  important theoretical restrictions.

}
\usage{
LHScorcorr(vars, COR=0, eps = 0.005, echo=FALSE, maxIt=0)
}
\arguments{
  \item{vars}{
	  The data.frame containing the parameters from the "raw" Latin Hypercube Sample.
	  Each column corresponds to one variable, and each line to one observation.
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
  Huntington, D.E. and Lyrintzis, C.S. 1998 Improvements to and limitations of 
  Latin hypercube sampling. \emph{Prob. Engng. Mech.} 13(4): 245-253.

  Chalom, A. and Prado, P.I.K.L. 2012. Parameter space exploration of ecological models
  \emph{arXiv}:1210.6278 [q-bio.QM]
}
