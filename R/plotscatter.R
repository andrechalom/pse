plotscatter <-
	function (vars, res=NULL, index.data=NULL, index.res=NULL, add.lm=TRUE, ylab = NULL, ...) {
		if (class(vars)=="LHS" | class(vars)=="PLUE") {
			if (is.null(index.data)) index.data <- 1:get.ninputs(vars)
			if (is.null(index.res)) index.res <- 1:get.noutputs(vars)
			data=get.data(vars)
			results=get.results(vars)
			if(is.null(ylab)) ylab = vars$res.names[index.res]
			plotscatter(data[,index.data], results[,index.res], add.lm=add.lm, ylab = ylab, ...)
		} else {
			res <- as.data.frame(res)
			if (is.null(dim(vars))) {nplots <-1} else {nplots <- dim(vars)[2]}
			if (is.null(dim(res))) {nplots <-nplots*1} else {nplots <- nplots* dim(res)[2]}

			nl <- floor(sqrt(nplots))
			nc <- ceiling(nplots/nl)
			opar <- par(no.readonly=TRUE)
			par (mar=c(4,4,2,0.5), mfrow=c(nl, nc), pch='.')
			on.exit(par(opar))

			index.var <- 1
			index.res <- 1
			for (i in 1:nl) for (j in 1:nc) {
				this.ylab =names(res)[index.res]
				if (!is.null(ylab)) {
					if (length(ylab)==1) this.ylab=ylab
					else this.ylab=ylab[index.res]
				}
				
				oneplotscatter(res[,index.res],vars[, index.var], c(names(vars)[index.var],this.ylab), add.lm, ...)
				index.res <- index.res +1
				if (index.res > dim(res)[2]) {index.res <- 1; index.var <- index.var + 1}
				if (index.var > dim(vars)[2]) break;
			}
		}
	}

oneplotscatter <-
function(res, var, name, add.lm, ...) {
		plot(var,res, xlab=name[1], ylab= name[2], ...)
		if (add.lm) {
			l <- lm(res~var)
			if (!is.na(coefficients(l)[2])) 
					abline((lm(res~var)))
		}
}
