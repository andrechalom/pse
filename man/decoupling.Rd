\name{decoupling}
\alias{decoupling}
\alias{tell}

\title{Decoupling Simulations and Estimations}

\description{
  \code{tell} and \code{ask} are S3 generic methods for decoupling
  simulations and sensitivity measures estimations in the package
  `sensitivity'. In general, they are
  not used by the end-user for a simple \R model, but rather for an
  external computational code. The LHS object implements only the 
  \code{tell} method. For help on the other methods, see the help
  pages on the `sensitivity' package.
}

\usage{
tell(x, y = NULL, \dots)
}

\arguments{
  \item{x}{the incomplete LHS object.}
  \item{y}{a data.frame of model responses.}
  \item{\dots}{currently not supported.}
}

\value{
  \code{tell} returns the LHS object with the results data.frame.
  \strong{Note: the methods in `sensitivity' alter the original
	  object as a side effect. This is NOT the case with the pse 
	  methods.}
}
\author{Original code by Gilles Pujol, LHS method implemented by Andre Chalom}
