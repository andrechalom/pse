\name{sbma}
\alias{sbma}
\title{
	Calculates the Symetrized Blest Measure of Agreement between two samples
}
\description{
	The Symetrized Blest Measure of Agreement is an alternative measure of
	rank correlation (similar to Kendall's Tau and Spearman's Rho). This
	function calculates the SBMA between two samples
}
\usage{
sbma(sample1, sample2, ...)
}
\arguments{
  \item{sample1}{
	  The first vector or LHS object to be compared.
  }
  \item{sample2}{
	  The second vector or LHS object to be compared.
}
  \item{...}{
	  Additional arguments. 
}
}
\references{
  Maturi, T.A. and Elsayigh, A. 2010. A comparison of correlation coefficients 
  via a three-step bootstrap approach. \emph{Journal of Mathematics Research} 2(2): 3-10.
}
