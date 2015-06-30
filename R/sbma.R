sbma <- function (sample1, sample2, ...) UseMethod("sbma")

sbma.default <- function (sample1, sample2, absolute=TRUE, ...) {
	x1 <- sample1; x2<-sample2;
	if (absolute) {x1 <- abs(x1); x2 <- abs(x2);}
	if (length(x1) != length(x2))
		stop("Sample sizes must be the same!");
	n <- length(x1);
	R <- rank(x1);
	S <- rank(x2);
	v1 <- -(4*n+5)/(n-1);
	v2 <- 6/(n^3-n);
	sbc <- v1+v2*(sum(R*S*(4-(R+S)/(n+1))));
	return (sbc)
}

sbma.LHS <- function(sample1, sample2, absolute=TRUE, ...) {
#	if(is.na(sample1$prcc) | is.na(sample2$prcc)) 
#		stop("At least one of the LHS objects is incomplete.\nTry using 'tell' before calculation the sbma.") 
	sb <- array();
	for (i in 1:dim(get.results(sample1))[2])
		sb[i] <- sbma(sample1$prcc[[i]]$PRCC$original, sample2$prcc[[i]]$PRCC$original);
	return (sb);
}   
