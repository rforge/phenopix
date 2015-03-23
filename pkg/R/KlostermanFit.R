KlostermanFit <-
function (ts, which='light',uncert=FALSE, nrep=100) {
		if (class(index(ts))[1]=='POSIXct') {
		doy.vector <- as.numeric(format(index(ts), '%j'))
		index(ts) <- doy.vector
	}
	# decide which function to use and fix names
	# later to be removed
	if (which=='heavy') the.function <- FitDoubleLogKlHeavy
	if (which=='light') the.function <- FitDoubleLogKlLight
	# if (which=='complete') the.function <- FitDoubleLogKlosterman	
	fit <- the.function(ts)
##	residuals <- as.vector(fit$predicted)-ts 
	residuals <- ts - as.vector(fit$predicted)
	# min.res <- min(residuals, na.rm=T)
	# max.res <- max(residuals, na.rm=T)
	sd.res <- sd(residuals, na.rm=TRUE)
	## absolute values to compute weights
	res2 <- abs(residuals)
	## normalized between 0 and 1, get weights for random noise generation 
	res3 <- res2/max(res2, na.rm=T)
	sign.res <- sign(residuals)
	# res.range <- range(residuals, na.rm=TRUE)
	# mean.res <- mean(residuals, na.rm=TRUE)
	if (uncert) {
		predicted.df <- data.frame(matrix(ncol=nrep, nrow=length(ts)))
		params.df <- data.frame(matrix(ncol=nrep, nrow=length(fit$params)))
		for (a in 1:nrep) {
			noise <- runif(length(ts), -sd.res, sd.res)		
			sign.noise <- sign(noise)
			pos.no <- which(sign.res!=sign.noise)
			if (length(pos.no)!=0) noise[pos.no] <- -noise[pos.no]
			# noise <- runif(length(ts), min.res, max.res)*(res3*3)			
			# randomly sample
			noised <- ts + noise
			fit.tmp <- the.function(noised)
			predicted.df[,a] <- fit.tmp$predicted
			params.df[,a] <- fit.tmp$params
			ratio <- a/nrep*100
			print(paste('computing uncertainty: ', ratio, '% done', sep=''))
		}
		predicted.df <- zoo(predicted.df, order.by=index(ts))
		rownames(params.df) <- names(fit$params)
		# tmp.df <- cbind(as.vector(fit$predicted), tmp.df)
		# names(tmp.df)[1] <- 'fitted'
		uncertainty.list <- list(predicted=predicted.df, params=params.df)
		returned <- list(fit=fit, uncertainty=uncertainty.list)
	return(returned)	
	} else {
returned <- list(fit=fit, uncertainty=NULL)
		(return(returned))
	}
}
