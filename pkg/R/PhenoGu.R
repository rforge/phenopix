PhenoGu <-
function(
	##title<< 
	## Method 'Deriv' to calculate phenology metrics
	##description<<
	## This function implements the derivative method for phenology. This is rather an internal function; please use the function \code{\link{Phenology}} to apply this method.
	
	x, 
			
	fit, 
	uncert=FALSE, 
    breaks,
	
	
	...

) {
	
    if (is.null(x)) {
        x <- fit$predicted
        spline.eq <- smooth.spline(x, df=length(x))
        der1 <- predict(spline.eq, d=1)$y
        t <- index(x)
        days <- index(x)
        values <- as.vector(x)        
    } else {
    names(x) <- names(fit$params)
	retrieved.formula <- fit$formula
	days <- index(fit$predicted)
    t <- index(fit$predicted)
	values <- as.vector(fit$predicted)
	D1 <- D(retrieved.formula, 't')
	D2 <- D(D1, 't')
    ## e1 <- parent.frame()
    der1 <- eval(D1, envir=as.list(x))
}
#     if (length(which(is.na(der1)==TRUE))!=0 | length(which(is.infinite(der1)==TRUE))!=0) {
#     der1[is.na(der1)] <- 0
#     der1[is.infinite(der1)] <- 0   
#     warning('Check your fitting because the first derivative contains NA or infinite values \n They were set at 0!')
# }
    if (length(which(is.na(der1)==TRUE))!=0 | length(which(is.infinite(der1)==TRUE))!=0) {metrics <- rep(NA, 9)} else {
	## extract parameters
	parameters <- fit$params
	# get statistical values
    prr <- max(der1, na.rm=T)
    ## peak recovery date
    prd <- days[which.max(der1)]
    ## peak senescence rate
    psr <- min(der1, na.rm=T)
    ## peak senescence date
    psd <- days[which.min(der1)]
    ## gcc @ prd
    y.prd <- values[which(days==prd)]
    ## time peak recovery rate
    # tPRD <- parameters['t01'] + parameters['b1']*log(parameters['c1'])
    # tPSD <- parameters['t02'] + parameters['b2']*log(parameters['c2'])   
    # ## recovery line
    rl.y <- prr*days+y.prd-prr*prd
    rl.eq <- lm(rl.y~days)
    ## gcc @ psd
    y.psd <- values[which(days==psd)]
    ## senenscence line
    sl.y <- psr*days+y.psd-psr*psd
    sl.eq <- lm(sl.y~days)
    baseline <- min(values, na.rm=T)
    maxline <- max(values, na.rm=T)
    ## upturn day is the intersection between rl and x axis
    UD <- (baseline-rl.eq$coefficients[1])/rl.eq$coefficients[2]
    ## recession day is the intersection between sl and x axis
    RD <- (baseline-sl.eq$coefficients[1])/sl.eq$coefficients[2]
    ## stabilization day, intersection between maxline and rl
    SD <- (maxline-rl.eq$coefficients[1])/rl.eq$coefficients[2]
    ## downturn day, intersection between maxline and sl
    DD <- (maxline-sl.eq$coefficients[1])/sl.eq$coefficients[2]
    			    ## subset data between SD and DD
    sub.time <- days[which(days>=SD & days<=DD)]
    sub.gcc <- values[which(days>=SD & days<=DD)]
    if (length(sub.time)>3) {
        ##compute a linear fit
        plateau.lm <- lm(sub.gcc~sub.time)
        M <- matrix( c(coef(plateau.lm)[2], coef(sl.eq)[2], -1,-1), nrow=2, ncol=2 )
        intercepts <- as.matrix( c(coef(plateau.lm)[1], coef(sl.eq)[1]))
        interception <- -solve(M) %*% intercepts
        DD <- interception[1,1]
    }
    ## calculate area under the curve
    # cut.x <- days[which(days>=UD & days<=RD)]
    # cut.y <- offset.y[which(days>=UD & days<=RD)]
    # the.fun <- function(t) {eval(retrieved.formula, envir=as.list(params))}
    plateau.slope <- try(plateau.lm$coefficients[2], silent=TRUE)
    plateau.intercept <- try(plateau.lm$coefficients[1], silent=TRUE)
    if (class(plateau.slope)=='try-error') {
        plateau.slope <- NA
        plateau.intercept <- NA
        }
	metrics <- c(UD, SD, DD, RD, maxline, baseline, prr, psr, plateau.slope)
    if (length(which(diff(metrics[1:4])<0)!=0))  {
        metrics <- rep(NA, 9)
        warning('Threshold do not respect expected timing: set to NA')
    }
}
	names(metrics)  <- c('UD', 'SD', 'DD', 'RD', 'maxline', 'baseline', 'prr', 'psr', 'plateau.slope')
	return(metrics)
}
