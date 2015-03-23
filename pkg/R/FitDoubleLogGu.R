FitDoubleLogGu <-
function(
    ##title<<
    ## Fit a double logisitic function to a vector according to Elmore et al. (2012)
    ##description<<
    ## This function fits a double logistic curve to observed values using the function as described in Elmore et al. (2012) (equation 4).

    x,
### vector or time series to fit

    t = index(x),
### time steps

    tout = t,
### time steps of output (can be used for interpolation)

    ...
### further arguments (currently not used)

    ##references<<
    ## Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674.

    ##seealso<<
    ## \code{\link{TSGFdoublelog}}, \code{\link{Phenology}}

    ) {
        if (class(index(x))[1]=='POSIXct') {
        doy.vector <- as.numeric(format(index(x), '%j'))
        index(x) <- doy.vector
    }
  n <- length(na.omit(x))


                                        # get statistical values
    n <- length(x)
    avg <- mean(x, na.rm=TRUE)
    mx <- max(x, na.rm=TRUE)
    mn <- min(x, na.rm=TRUE)
    ampl <- mx - mn
                                            # double logistic function
    .doubleLog <- function(par, t) {
        y0=par[1]
        a1 <- par[2]
        a2 <- par[3]
        t01 <- par[4]
        t02 <- par[5]
        b1 <- par[6]
        b2 <- par[7]
        c1 <- par[8]
        c2 <- par[9]
        ## m1 <- par[1]
        ## m2 <- par[2]
        ## m3 <- par[3]
        ## m4 <- par[4]
        ## m5 <- par[5]
        ## m6 <- par[6]
        ## m7 <- par[7]
        ## m3l <- m3 / m4
        ## m4l <- 1 / m4
        ## m5l <- m5 / m6
        ## m6l <- 1 / m6
        # xpred <- offset + (ampl/(1+q1*exp(-b1*(t-m1)))^v1 -
        #          ampl/(1+q2*exp(-b2*(t-m2)))^v2)
        ## xpred <- m1 + (m2 - m7 * t) * ( (1/(1 + exp((m3l - t)/m4l))) - (1/(1 + exp((m5l - t)/m6l))) )
                                        # term a1*t + b1 must be close to the minimum
                                        # term a2*t^2 + b2*t + c must be close to seasonal ampl
    xpred <- y0 + (a1/(1+exp(-(t-t01)/b1))^c1) - (a2/(1+exp(-(t-t02)/b2))^c2)
        # xpred <- (a1*t + b1) + (a2*t^2 + b2*t + c)*(1/(1+q1*exp(-B1*(t-m1)))^v1 - 1/(1+q2*exp(-B2*(t-m2)))^v2)
        return(xpred)
    }

                                        # error function
    .error <- function(par, x, t) {
        if (any(is.infinite(par))) return(99999)
        xpred <- .doubleLog(par, t=t)
        sse <- sum((xpred - x)^2, na.rm=TRUE)
        return(sse)
    }

                                        # inital parameters to fit double-logistic function
    doy <- quantile(t, c(0.25, 0.75), na.rm=TRUE)
    # doy2 <- diff(doy)
    y0 <- mn
    a1 <- ampl #ok
    a2 <- ampl #ok
    tmp <- smooth.spline(x, df=0.5*length(x))
    doy.max <- which.max(tmp$y)
    t01 <- doy[1] + 0.5*(doy.max-doy[1]) 
    t02 <- doy.max + 0.5*(doy[2]-doy.max) 
    b1 <- 10 #ok
    b2 <- 10 #ok
    c1 <- 1 # ok 
    c2 <- 1
  
    prior <- rbind(
        # c(offset, ampl, b1, b2, m1, m2, q1, q2, v1, v2),
        # c(offset, ampl, 0.03, b2, m1, m2, q1, q2, v1, v2),
        # c(offset, ampl, b1, b2, m1, m2, q1, q2, v1, v2),
        # c(offset, ampl, 0.03, b2, m1, m2, 2, 2, v1, v2)        
        c(y0, a1, a2, t01, t02, b1, b2, c1, c2), 
        c(y0, a1, a2, t01, t02, b1, b2, 1.2, c2), 
        c(y0, 0.05, 0.05, t01, t02, 0.5, b2, c1, c2), 
        c(y0, a1, a2, doy[1], t02, b1, b2, c1, c2),         
        c(y0, a1, a2, t01, doy[2], 5, 5, c1, c2)
        )

                                        # estimate parameters for double-logistic function starting at different priors
    opt.l <- apply(prior, 1, optim, .error, x=x, t=t, method="BFGS", control=list(maxit=1000))   # fit from different prior values
    opt.df <- cbind(cost=unlist(llply(opt.l, function(opt) opt$value)), convergence=unlist(llply(opt.l, function(opt) opt$convergence)), ldply(opt.l, function(opt) opt$par))
    ## opt.df <- opt.df[-which(opt.df$V2<0),]
    best <- which.min(opt.df$cost)

                                        # test for convergence
    if (opt.df$convergence[best] == 1) { # if maximum iterations where reached - restart from best with more iterations
        opt <- opt.l[[best]]
        opt <- optim(opt.l[[best]]$par, .error, x=x, t=t, method="BFGS", control=list(maxit=1500))
        prior <- rbind(prior, opt$par)
        xpred <- .doubleLog(opt$par, t)
    } else if (opt.df$convergence[best] == 0) {
        opt <- opt.l[[best]]
        prior <- rbind(prior, opt$par)
        xpred <- .doubleLog(opt$par, t) ## perche questo restituisce nan?
    }

                                        # plot iterations
    # if (plot) {
    #     llply(opt.l, function(opt) {
    #         xpred <- .doubleLog(opt$par, t)
    #         lines(t, xpred, col="cyan")
    #     })
    #     lines(t, xpred, col="blue", lwd=2)
    # }

                                        # return NA in case of no convergence
    if (opt$convergence != 0) {
        opt$par[] <- NA
        xpred <- rep(NA, length(tout))
    } else {
        xpred <- .doubleLog(opt$par, tout)
    }
 xpred.out <- zoo(xpred, order.by=t)
names(opt$par) <- c('y0', 'a1', 'a2', 't01', 't02', 'b1', 'b2', 'c1', 'c2')
fit.formula <- expression(y0 + (a1/(1+exp(-(t-t01)/b1))^c1) - (a2/(1+exp(-(t-t02)/b2))^c2))
output <- list(predicted=xpred.out, params=opt$par, formula=fit.formula)
return(output)
    # if (return.par) {
    #     ## names(opt$par) <- paste("m", 1:7, sep="")
    #     return(opt$par)
    # } else {
    #     return(xpred)
    # }
}
