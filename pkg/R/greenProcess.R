greenProcess <- function(ts, fit, threshold=NULL, plot=TRUE, which='light', uncert=FALSE, nrep=100, 
    envelope='quantiles', quantiles=c(0.1, 0.9), ...) {
if (missing(fit)) stop('Provide a fit name')	
if (fit=='spline') fit.fun <- SplineFit
if (fit=='beck') fit.fun <- BeckFit
if (fit=='elmore') fit.fun <- ElmoreFit
if (fit=='klosterman') fit.fun <- KlostermanFit
if (fit=='gu') fit.fun <- GuFit
if (fit=='klosterman') fitted.data <- try(fit.fun(ts, uncert=uncert, which=which, nrep=nrep)) else {
fitted.data <- try(fit.fun(ts, uncert=uncert, nrep=nrep))
}
if (is.null(threshold)) {
	metrics <- NULL 
if (plot) plot(fitted.data$fit$predicted, ...)
}else {
metrics <- PhenoExtract(fitted.data, method=threshold, envelope=envelope, quantiles=quantiles, uncert=uncert, 
    plot=plot, ...)  
}
output <- list(fit=fitted.data, metrics=metrics$metrics, data=ts, uncertainty.df=metrics$unc.df)
## set structure and attributes to output for summary, print, etc
info <- list(fit=fit, threshold=threshold, uncert=uncert, envelope=envelope, 
	quantiles=quantiles, nrep=ifelse(uncert, nrep, 0))
info[7] <- attributes(output)
names(info)[7] <- 'names'
attributes(output) <- info
class(output) <- 'phenopix'
return(output) 
}
