\name{pic}
\alias{pic}
\alias{print.pic}
\title{
	Partial Inclination Coefficient
}
\description{
	Estimates the partial inclination coefficient of a model response
	in relation with all model input variables.
}
\usage{
pic(X, y, nboot, conf, \dots)
\method{print}{pic}(x, \dots)
}
\arguments{
  \item{X}{a data frame (or object coercible by \code{as.data.frame})
    containing the design of experiments (model input variables).}
  \item{y}{a vector containing the responses corresponding to the design
    of experiments (model output variables).}
  \item{nboot}{
	  Number of bootstrap replicates
}
  \item{conf}{
	  Confidence of the bootstrap interval
}
  \item{x}{the object returned by \code{pcc}.}
\item{\dots}{Currently ignored}
}

\source{
  based on code of pcc function on sensitivity package
  by Gilles Pujol, Bertrand Iooss, Alexandre Janon.
}

\author{Andre Chalom, based on code by Gilles Pujol, Bertrand Iooss, Alexandre Janon}      
