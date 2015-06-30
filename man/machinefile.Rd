\name{machinefile}
\alias{machinefile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Convenience for clustering
%%  ~~function to do ... ~~
}
\description{
	Provides a convenience interface for using MPD-style hostfiles to generate cluster objetcs.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
machinefile(name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
	  Filename of the hostfile.
%%     ~~Describe \code{name} here~~
}
}
\details{
	The hostfile should be written as a text file using the MPD style:
	one line for each host, which can be followed by a colon and a number
	indicating the number of processes to be started on that host. An example
	hostfile for starting three processes on two hosts named avalon and
	glastonbury would be:

	avalon
	glastonbury:2
%%  ~~ If necessary, more details than the description above ~~
}
\examples{
	\dontrun{
		library(parallel)
		cl = makePSOCKcluster(machinefile("mpd.hosts"))
		stopCluster(cl)
	}
}
