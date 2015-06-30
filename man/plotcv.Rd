\name{plotcv}
\alias{plotcv}
\title{
	Coefficient of variation plot
}
\description{
	The \code{plotcv} function plots the empirical cummulative density function 
	(ecdf) of the coefficient of variation of the LHS resulting from a stochastic 
	simulation, along with a dotted line representing the coefficient of variation
	of the whole result set.
}
\usage{
plotcv(x, stack = FALSE, index.res = 1, col = index.res, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	  An LHS model with more than one repetition.
}
  \item{stack}{
	  Warning: not implemented in this version!
}
  \item{index.res}{
	  Warning: not implemented in this version!
}
  \item{col}{
	  The color of the plot.
}
  \item{\dots}{
	  Other parameters to be passed to the lower level plotting function.
}
}
