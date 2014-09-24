convert <- function(x) {
	if (class(x)!='ts' |  class(x)!='zoo')
	time.new <- index(x)
	x.df <- as.data.frame(x)
	rownames(x.df) <- NULL
	x.df$time <- time.new
	return(x.df)
}