# Based on code by Gilles Pujol 2006
# Released originally as package sensitivity, licensed as GPL-2
### WORK IN PROGRESS: adaptar as funcoes para multiplas respostas
estim.pic <- function(data, i = 1:nrow(data)) {  
  d <- data[i, ]
  p <- ncol(d) - 1
  pic <- numeric(p)
  for (j in 1:p) {
    Xtildej.lab <- paste(colnames(d)[c(-1, -j-1)], collapse = "+")
    lm.Y <- lm(formula(paste(colnames(d)[1], "~", Xtildej.lab)), data = d)
    lm.Xj <- lm(formula(paste(colnames(d)[j+1], "~", Xtildej.lab)), data = d)
	y = d[1] - fitted(lm.Y)
	x = d[j+1] - fitted(lm.Xj)
	pic[j] <- coef(lm(y[,1] ~ x[,1]))[2]
  }
  return(pic)
}

pic <- function (X, y, nboot, conf, ...) UseMethod("pic")
pic.LHS <- function (X, y=NULL, nboot=0, conf=0.95, ...) {
	res <- get.results(X)
	L <- get.data(X)
	f <- function(r) pic(X=L, y=r, nboot, conf, ...)
	apply(res, 2, f)
}
pic.default <- function (X, y, nboot = 0, conf=0.95, ...) {
	data <- cbind(Y=y, X)
	if (nboot == 0) {
		pic <- data.frame(original=estim.pic(data))
		rownames(pic) <- colnames(X)
	} else {
		boot.pic <- boot(data, estim.pic, R = nboot)
		pic <- bootstats(boot.pic, conf, "basic")
		rownames(pic) <- colnames(X)
	}
	out <- list(X = X, y = y, nboot = nboot, conf = conf,
				call = match.call(), pic = pic)
	class(out) <- "pic"
	return(out)
}

print.pic <- function(x, ...) {
  cat("\nCall:\n", deparse(x$call), "\n", sep = "")
    cat("\nPartial Inclination Coefficients (PIC):\n")
    print(x$pic)
}
