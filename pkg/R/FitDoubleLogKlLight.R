FitDoubleLogKlLight <-
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

    ...

    ) {
    n <- length(x)
    avg <- mean(x, na.rm=TRUE)
    mx <- max(x, na.rm=TRUE)
    mn <- min(x, na.rm=TRUE)
    ampl <- mx - mn
    .doubleLog <- function(par, t) {
        
        a1 <- par[1]
        a2 <- par[2]
        b1 <- par[3]
        b2 <- par[4]
        c <- par[5]
        B1 <- par[6]
        B2 <- par[7]
        m1 <- par[8]
        m2 <- par[9]
        q1 <- par[10]
        q2 <- par[11]
        v1 <- par[12]
        v2 <- par[13]
 
        xpred <- (a1*t + b1) + (a2*t^2 + b2*t + c)*(1/(1+q1*exp(-B1*(t-m1)))^v1 - 1/(1+q2*exp(-B2*(t-m2)))^v2)
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
    a1 <- 0 #ok
    a2 <- 0 #ok
    b1 <- mn #ok
    b2 <- 0 #ok
    c <- 0.2*max(x) # ok 
    ## very slightly smoothed spline to get reliable maximum
    tmp <- smooth.spline(x, df=0.5*length(x))
    doy.max <- which.max(tmp$y)
    B1 <- 4/(doy.max-doy[1])
    B2 <- 3.2/(doy[2]-doy.max)
    m1 <- doy[1] + 0.5*(doy.max-doy[1])
    m2 <- doy.max + 0.5*(doy[2]-doy.max)
    m1.bis <- doy[1]
    m2.bis  <- doy[2]  
    q1 <- 0.5 #ok
    q2 <- 0.5 #ok
    v1 <- 2 # ok
    v2 <- 2 # ok


    prior <- rbind(
        # c(offset, ampl, b1, b2, m1, m2, q1, q2, v1, v2),
        # c(offset, ampl, 0.03, b2, m1, m2, q1, q2, v1, v2),
        # c(offset, ampl, b1, b2, m1, m2, q1, q2, v1, v2),
        # c(offset, ampl, 0.03, b2, m1, m2, 2, 2, v1, v2)        
        c(a1, a2, b1, b2, c, B1, B2, m1,m2, q1, q2, v1, v2),
        c(a1, a2, b1, 0.01, 0, B1, B2, m1,m2.bis, q1, 1, v1, 4),
        c(a1, a2, b1, b2, c, B1, B2, m1.bis,m2, q1, q2, v1, v2),
        c(a1, a2, b1, b2, c, B1, B2, m1,m2.bis, q1, q2, v1, v2),
        c(a1, a2, b1, b2, c, B1, B2, m1.bis,m2, q1, q2, v1, v2)
        )

    # if (plot) plot(t, x)

                                        # estimate parameters for double-logistic function starting at different priors
    opt.l <- apply(prior, 1, optim, .error, x=x, t=t, method="BFGS", control=list(maxit=1000))   # fit from different prior values
    opt.df <- cbind(cost=unlist(llply(opt.l, function(opt) opt$value)), convergence=unlist(llply(opt.l, function(opt) opt$convergence)), ldply(opt.l, function(opt) opt$par))
    ## remove negative exponents for q1 and 2, var 10 and 11
    pos <- which(opt.df$V6 < 0 | opt.df$V7 < 0)
    if (length(pos)!=0) {
        opt.df <- opt.df[-pos,]
        opt.l <- opt.l[-pos]
    }
    best <- which.min(opt.df$cost)

                                        # test for convergence
    if (opt.df$convergence[best] == 1) { # if maximum iterations where reached - restart from best with more iterations
        opt <- opt.l[[best]]
        opt <- optim(opt.l[[best]]$par, .error, x=x, t=t, method="BFGS", control=list(maxit=700))
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
names(opt$par) <- c('a1', 'a2', 'b1', 'b2', 'c', 'B1', 'B2', 'm1', 'm2', 'q1', 'q2', 'v1', 'v2') 
fit.formula <- expression((a1*t + b1) + (a2*t^2 + b2*t + c)*(1/(1+q1*exp(-B1*(t-m1)))^v1 - 1/(1+q2*exp(-B2*(t-m2)))^v2))
output <- list(predicted=xpred.out, params=opt$par, formula=fit.formula)
return(output)
    # if (return.par) {
    #     ## names(opt$par) <- paste("m", 1:7, sep="")
    #     return(opt$par)
    # } else {
    #     return(xpred)
    # }
}
