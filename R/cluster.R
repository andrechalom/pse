machinefile <- function(name) {
	x <- read.table(name, sep=":", header=FALSE, stringsAsFactors=FALSE, fill=TRUE)
	ret <- c()
	for (i in 1:(dim(x)[1])) { 
		if (is.null(x[i,2]) | is.na (x[i,2])) x[i,2] <- 1
		ret <-c(ret, rep(x[i,1], as.numeric(x[i,2])))
	}
	return(ret)
}

clusterRun <- function (cl, model, L) {
	N <- dim(L)[1]
	sp <- clusterSplit(cl, 1:N)
	tmp.res <- clusterApply(cl, sp, 
			 fun = function(idx, x) model(x[idx,]), L)
	n.outs <- length(unlist(tmp.res))/N
	res <- array( dim=c(N, n.outs));
	for (i in 1:(length(sp))) 
		res[sp[[i]], 1:n.outs] <- t(tmp.res[[i]])
	return (res)
}

