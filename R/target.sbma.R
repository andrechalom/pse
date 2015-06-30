target.sbma <- function(target, model, factors,  q = NULL, q.arg = NULL, res.names=NULL, method=c("HL", "random"), 
						opts=list(), init=length(factors)+2, inc=100, FUN=min) {
	#initial LHS
	N = init
	method=match.arg(method)
	print("INFO: initial run...")
	oldL <- LHS(model, factors, N, q, q.arg, res.names, method, opts, nboot=0)
	while (TRUE) {
		N = N + inc
		print(paste("INFO: LHS with N =", N));
		newL <- LHS(model, factors, N, q, q.arg, res.names, method, opts, nboot=0)
		s <- FUN(sbma(newL, oldL))
		print(paste("sbma of ", round(s,3)," (target ",target,")", sep=""))
		if (s >= target) return (newL);
		oldL <- newL;
	}
}

