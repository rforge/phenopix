BeckFit <-
function (ts, uncert=FALSE, nrep=100) {
	fit <- FitDoubleLogBeck(ts)
	residuals <- as.vector(fit$predicted)-ts 
	# res.range <- range(residuals, na.rm=TRUE)
	# mean.res <- mean(residuals, na.rm=TRUE)
	sd.res <- sd(residuals, na.rm=TRUE)
	## absolute values to compute weights
	res2 <- abs(residuals)
	## normalized between 0 and 1, get weights for random noise generation 
	res3 <- res2/max(res2)
	sign.res <- sign(residuals)
if (uncert) {
		predicted.df <- data.frame(matrix(ncol=nrep, nrow=length(ts)))
		params.df <- data.frame(matrix(ncol=nrep, nrow=length(fit$params)))
		for (a in 1:nrep) {
			noise <- rnorm(length(ts), 0, sd.res)*(res3*3)
			sign.noise <- sign(noise)
			pos.no <- which(sign.res!=sign.noise)
			if (length(pos.no)!=0) noise[pos.no] <- -noise[pos.no]
			# randomly sample
			noised <- ts + noise
			fit.tmp <- FitDoubleLogBeck(noised)
			predicted.df[,a] <- fit.tmp$predicted
			params.df[,a] <- fit.tmp$params
			ratio <- a/nrep*100
			print(paste('computing uncertainty: ', ratio, '% done', sep=''))
		}
		predicted.df <- zoo(predicted.df, order.by=index(ts))
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
