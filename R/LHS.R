LHS <-
	function (model=NULL, factors, N, q=NULL, q.arg=NULL, res.names=NULL, method=c("HL", "random"),
			  opts=list(), nboot=0, repetitions=1, cl = NULL) {
		# Input validation for common errors and "default" value handling:
		method = match.arg(method)
		my.opts = list(COR=0, eps=0.0005)
		my.opts[names(opts)] <- opts
		if(is.numeric(factors) && length(factors) == 1) factors=paste("I", 1:factors, sep="")
		else if (!is.character(factors)) {
			stop("Error in function LHS: factors should be either a single number or a character vector")
		}
		if (!is.numeric(N) || length(N) != 1) {
			stop("Error in function LHS: N should be a single number");
		}
		if (N < length(factors) + 2) {
			stop("Error in function LHS: the number of points N must be at least the number of factors + 2");
		}
		# "Defaults" for q and q.arg
		if (is.null(q)) q=rep("qunif", length(factors)) 
		else if (length(q)==1)  q=rep(q, length(factors))
		if (is.null(q.arg)) q.arg =rep( list(list()), length(factors))
		else if (FALSE %in% sapply(q.arg, is.list)) q.arg <- rep(list(q.arg), length(factors))

		# Generates the hypercube data
		L <- as.data.frame(matrix(nrow=N, ncol=length(factors)));
		colnames(L) <- factors
		for (i in 1:length(factors)) 
			L[,i] <- sample(do.call(q[[i]], c(list(p = 1:N/N-1/N/2), q.arg[[i]])))
		# Corrects the correlation terms, for HL method for LHS
		if (method == "HL") {
			L <- LHScorcorr(L, COR = my.opts$COR, eps = my.opts$eps); 
		}
		# Runs the actual model
		res <- internal.run(cl, model, L, repetitions)
		prcc <- internal.prcc(L, res, nboot)

		if (is.null(res.names) && ! is.na(res)) res.names <- paste("O", 1:dim(res)[2], sep="")
		X <- list(call=match.call(), N=N, data=L, factors=factors, q=q, q.arg=q.arg, 
				  opts = my.opts, model=model, res=res, prcc=prcc,
				  res.names=res.names);
		class(X) <- "LHS"
		return(X);
	}

internal.run <- function(cl, model, L, repetitions) {
	if (is.null(model)) {return(NA)}
	# First run, is independent of "repetitions"
	if (is.null(cl)) {
		tmp.res <- t(model(L));
		if(dim(tmp.res)[1] == 1) tmp.res = t(tmp.res)
	}
	else {
		tmp.res <- clusterRun(cl, model, L)
	}
	# and tells us the number of model outputs
	n.outs <- dim(tmp.res)[2]
	N <- dim(L)[1]
	res <- array(tmp.res, dim=c(N, n.outs, repetitions));
	if(repetitions> 1) for (i in 2:repetitions) {
		if (is.null(cl)) res[,,i] <- t(model(L))
		else res[,,i] <- clusterRun(cl, model, L)
	}
	return(res)
}


##Methods

print.LHS <- function(x, ...) {
	  cat("\nCall:\n", deparse(x$call), "\n", sep = "")
	  cat("Model:\n"); print (x$model);
	  cat("Factors:\n"); print (x$factors);
	  cat("Results:\n"); print (x$res.names);
	  cat("PRCC:\n"); print (x$prcc);
}

tell <- function(x, y = NULL, ...)
	  UseMethod("tell")

tell.LHS <- function (x, y, res.names=NULL, nboot=0, ...) {
	tmp.res <- t(y);
	if(dim(tmp.res)[1] == 1) tmp.res = t(tmp.res)
	# If this is the first "tell"
	if(all(is.na(get.results(x)))) {
		n.outs <- dim(tmp.res)[2]
		res <- array(tmp.res, dim=c(x$N, n.outs, 1));
	} else {
		this.repetition = get.repetitions(x)+1
		res <- array(get.results(x, get.mean=FALSE), dim=c(get.N(x), get.noutputs(x), this.repetition))
		res[,,this.repetition] = tmp.res
	}
	x$res <- res
	x$prcc <- internal.prcc(get.data(x), res, nboot)
	if (!is.null(res.names)) x$res.names <- res.names
	if (is.null(x$res.names)) x$res.names <- paste("O", 1:dim(res)[2], sep="")
	
	return(x)
}

internal.prcc <- function (L, res, nboot) {
	if (is.null(dim(res))) {return (NA)}
	# Reduces the res object to a 2-dimensional array
	res <- apply(res, c(1,2), mean)
	f <- function(r) pcc(L, r, nboot=nboot, rank=T)
	return(apply(res, 2, f))
}

