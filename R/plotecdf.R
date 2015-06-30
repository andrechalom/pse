plotecdf <- function (obj, stack=FALSE, index.res =1:get.noutputs(obj), col=index.res, xlab = NULL, ...) {
	if (is.null (xlab)) xlab = obj$res.names
	if (stack) {
		if (length(xlab) > 1) xlab = "obj results"
		dat <- vec(get.results(obj)[,index.res])
		g <- rep(index.res, each=dim(obj$res)[1])
		Ecdf(dat, group=g, col=col, xlab=xlab, ...)
	} else Ecdf(get.results(obj)[,index.res], xlab=xlab, ...)
}

