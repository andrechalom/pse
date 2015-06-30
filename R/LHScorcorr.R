LHScorcorr <-
	function (vars, COR = 0, eps = 0.005, echo=FALSE, maxIt = 0) {
		if (! is.matrix(COR)) COR = matrix(0,dim(vars)[2],dim(vars)[2])
		if (maxIt==0) maxIt = 2*sqrt(dim(vars)[1])
		internal.LHScorcorr(vars, COR, 2, eps, 1, echo, maxIt)
	}

internal.LHScorcorr <- 
	function (vars, COR, l, eps, it, echo, maxIt) {
		N <- dim(vars)[1]; M <- dim(vars)[2]
		my.names <- colnames(vars)
		# Stop condition: 
		if (l == M + 1) {
			return (vars);
		}
		# Skipping this column: correlations are close enough to the prescribed
		if (max(abs(cor(vars)[l,1:(l-1)] - COR[l,1:(l-1)])) < eps) {
			return (internal.LHScorcorr (vars, COR, l = l + 1, eps = eps, it = 1, echo=echo, maxIt=maxIt));
		}
		# Skipping this column: maximum iterations for the same variable
		# If maxIt is set to -1, NEVER gives up
		if (it > maxIt | maxIt < 0) { 
			warning("LHScorcorr: correlation does not converge after maximum iterations");
			return (internal.LHScorcorr (vars, COR, l = l + 1, eps = eps, it = 1, echo=echo, maxIt=maxIt));
		}
		if (echo==T) cat(paste("Info: Correlation correction being made for l =",l,"/",M, "\n"))
		# Here we start correcting the correlation for var[,l]
		V <- .C(corcorr, vars=as.double(as.matrix(vars)),cor=as.double(COR), N=as.integer(N), M=as.integer(M), l=as.integer(l), FLAGSTOP=as.integer(0))
		vars <- as.data.frame(matrix(V$vars, nrow=N, ncol=M))
		names(vars) <- my.names
		if (V$FLAGSTOP == 1) { # Convergence, going for next
			return(internal.LHScorcorr (vars, COR, l = l + 1, eps = eps, it = 1, echo=echo, maxIt=maxIt));
		} else {
			# Repeat the proccess with the same variable
			internal.LHScorcorr(vars,COR,l=l,eps=eps,it=it+1, echo=echo, maxIt=maxIt)
		}
	}
