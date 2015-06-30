get.results <-
function(obj, get.mean=TRUE) { 
	# IF the object is incomplete (see tell method)
	if (is.null(dim(obj$res))) {return (NA)}

	if(get.mean) {
		return (apply(obj$res, c(1,2), mean)) 
	} else {
		return (obj$res) 
	}
}

get.data <-
function(obj) {
	return (obj$data) 
}

get.N <- function(obj) {return (dim(get.data(obj))[1]) }
get.ninputs <- function(obj) {return (dim(get.data(obj))[2]) }
get.noutputs <- function(obj) {return (dim(get.results(obj))[2]) }
get.repetitions <- function(obj) {return (dim(get.results(obj, get.mean=FALSE))[3]) }
