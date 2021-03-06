% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessors.R
\name{get.results}
\alias{get.results}
\alias{get.data}
\alias{get.N}
\alias{get.ninputs}
\alias{get.noutputs}
\alias{get.repetitions}
\title{LHS Accessor Functions.}
\usage{
get.results(obj, get.mean = TRUE)

get.data(obj)

get.N(obj)

get.ninputs(obj)

get.noutputs(obj)

get.repetitions(obj)
}
\arguments{
\item{obj}{The LHS or PLUE object}

\item{get.mean}{In case of stochastic models, when several model runs are required for the
same data point, the \code{data} slot of the LHS object contains all the 
model outputs. Use \code{get.mean=TRUE} to get the average values for each point,
or \code{get.mean=FALSE} to get all the results.}
}
\description{
Instead of using the $ operator, using these accessor functions 
is the preferred method for accessing the data and result
data frames from an \code{\link{LHS}} or \code{\link{PLUE}} object, 
 as the internal structure of
the object may vary between versions of the package.
}
\details{
\code{get.data} returns a data.frame consisting on the input data.

\code{get.results} returns an array with the model results. See the 
vignette on multiple runs for details on the \code{get.mean} argument.

\code{get.N}, \code{get.ninputs}, \code{get.noutputs} return a single number each,
with the number of points in the hypercube (or sampling), number of input factors and number of
response variables.

\code{get.repetitions} returns the number of model repetitions for each data point,
created by \code{LHS(model, factors, N, repetitions=X)}, or by \code{tell}ing several
result sets to the same LHS object (or PLUE object).
}
