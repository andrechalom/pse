#' @export
#' @param quant Maximum quantile to be plotted on the ecdf (used to cut off extreme values in the labels)
#' @rdname plots
plotcv <- function(obj, stack = FALSE, index.res = 1:get.noutputs(obj), col = index.res, quant = 0.99, ...) { 
    opar = par(no.readonly=TRUE)
    on.exit(par(opar))
	if (class(obj)!="LHS")
		stop("The first argument should be of class LHS!");
	if (get.repetitions(obj)<2)
		stop("Error in function plotcv: the LHS object must have at least two repetitions!")

	pointwise <- abs(apply(get.results(obj, FALSE), c(1,2), cv))
	global <- abs(apply(get.results(obj, TRUE), 2, cv))

    if (stack) {
        dat = vec(pointwise[,index.res])
        g = rep(index.res, each=get.N(obj))
        m <- max(quantile(pointwise[,index.res],quant), 1.05*global)
   	    mi <- min(pointwise[,index.res], global)
  	    Ecdf(dat, group = g, xlim=c(mi, m), xlab="pointwise cv", col=col, ...)
        for (i in index.res)
    	    abline(v=global[i], lwd=2, lty=3, col=col[i])
    	if (m > 0.8*max(pointwise)) {pos=2} else {pos=4}
        text(x=global[1], y=0.1, label="global cv", pos=pos)
    } else {
        nl = floor(sqrt(length(index.res)))
        nc = ceiling(length(index.res)/nl)
        par(mfrow=c(nl, nc))
        for (i in index.res) {
            m <- max(quantile(pointwise[,i],quant), 1.05*global)
            mi <- min(pointwise[,i], global)
            Ecdf(pointwise[,i], xlim=c(mi, m), xlab="pointwise cv", ...)
            abline(v=global[i], lwd=2, lty=3)
            if (m > 0.8*max(pointwise[,i])) {pos=2} else {pos=4}
            text(x=global[i], y=0.1, label="global cv", pos=pos)
        }
    }
}

#' Coefficient of Variation
#' 
#' Returns the coefficient of variation of a sample.
#' @param x Any numeric vector (or other data type for which \code{sd} and \code{mean} methods are defined)
#' @param \dots Additional parameters for the \code{sd} and \code{mean} functions (such as \code{na.rm=TRUE})
#' @export
cv <- function(x, ...) {
	if (mean(x, ...) == 0) return (0)
	else return (sd(x, ...)/mean(x, ...))
}
