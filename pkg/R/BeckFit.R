BeckFit <-
function (ts, uncert=FALSE, nrep=1000, error='average') {
	if (class(index(ts))[1]=='POSIXct') {
		doy.vector <- as.numeric(format(index(ts), '%j'))
		index(ts) <- doy.vector
	}
	if (uncert) hessian  <- TRUE else hessian  <- FALSE
	fit <- FitDoubleLogBeck(ts, hessian=hessian)
	# residuals <- ts - as.vector(fit$predicted) 
	# # res.range <- range(residuals, na.rm=TRUE)
	# # mean.res <- mean(residuals, na.rm=TRUE)
	# sd.res <- sd(residuals, na.rm=TRUE)
	# ## absolute values to compute weights
	# res2 <- abs(residuals)
	# ## normalized between 0 and 1, get weights for random noise generation 
	# res3 <- res2/max(res2)
	# sign.res <- sign(residuals)
	if (uncert) {
		.uncertainty <- function(fit, nrep=nrep, failed.error='average') {
			params <- fit$params
			std.errors <- fit$stdError
			pc.error <- mean(std.errors/params, na.rm=TRUE)
			if (failed.error !='average') corr.factor <- pc.error 
			if (is.numeric(failed.error)) corr.factor <- failed.error
			if (!exists('corr.factor')) corr.factor = 0  
			nrep <- nrep
			uncert.matrix <- data.frame(matrix(nrow=nrep, ncol=length(params)))
			names(uncert.matrix) <- names(params)
			for (i in 1:length(params)) {
				act.par <- params[i]
				act.err <- std.errors[i]
        # if (is.na(act.err)) act.err <- abs(0.01*act.par)
				if (is.na(act.err)) act.err <- abs(corr.factor*act.par)            
					uncert.matrix[,i] <- rnorm(nrep, act.par, act.err)        
			}
			uncert.df <- data.frame(matrix(nrow=length(index(fit$predicted)), ncol=nrep))
			t <- index(fit$predicted)
			for (a in 1:nrep) {
				act.pars <- uncert.matrix[a,]
				uncert.df[,a] <- eval(fit$formula, envir=as.list(act.pars))        
			}
			uncert.df.zoo <- zoo(uncert.df, order.by=index(fit$predicted))
			output.uncert <- list(predicted=uncert.df.zoo, params=uncert.matrix)
			output <- list(fit=fit, uncertainty=output.uncert)
			return(output)
		}

		unc.estimate <- .uncertainty(fit, nrep=nrep, failed.error=error)
		# predicted.df <- data.frame(matrix(ncol=nrep, nrow=length(ts)))
		# params.df <- data.frame(matrix(ncol=nrep, nrow=length(fit$params)))
		# for (a in 1:nrep) {
		# 	noise <- runif(length(ts), -sd.res, sd.res)
		# 	sign.noise <- sign(noise)
		# 	pos.no <- which(sign.res!=sign.noise)
		# 	if (length(pos.no)!=0) noise[pos.no] <- -noise[pos.no]
		# 	# randomly sample
		# 	noised <- ts + noise
		# 	fit.tmp <- FitDoubleLogBeck(noised)
		# 	predicted.df[,a] <- fit.tmp$predicted
		# 	params.df[,a] <- fit.tmp$params
		# 	ratio <- a/nrep*100
		# 	print(paste('computing uncertainty: ', ratio, '% done', sep=''))
		# }
		# predicted.df <- zoo(predicted.df, order.by=index(ts))
		# # tmp.df <- cbind(as.vector(fit$predicted), tmp.df)
		# # names(tmp.df)[1] <- 'fitted'
		# uncertainty.list <- list(predicted=predicted.df, params=params.df)
		# returned <- list(fit=fit, uncertainty=uncertainty.list)
		return(unc.estimate)	
	} else {
		returned <- list(fit=fit, uncertainty=NULL)
		(return(returned))
	}
}
