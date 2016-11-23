FitDoubleLogKlLight <-
function(
    x,
    t = index(x),
    tout = t,
    hessian = FALSE,
    sf=quantile(x, probs=c(0.05, 0.95), na.rm=TRUE),
    ...
    ) {
    .normalize <- function(x, sf) (x-sf[1])/(sf[2]-sf[1])
        .backnormalize <- function(x, sf) (x+sf[1]/(sf[2]-sf[1]))*(sf[2]-sf[1])    
        if (any(is.na(x))) stop('NA in the time series are not allowed: fill them with e.g. na.approx()')
        if (class(index(x))[1]=='POSIXct') {
            doy.vector <- as.numeric(format(index(x), '%j'))
            index(x) <- doy.vector
            t <- index(x)
            tout <- t
        }
        n <- length(x)
        x <- .normalize(x, sf=sf)
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
        .error <- function(par, x, t) {
            if (any(is.infinite(par))) return(99999)
                xpred <- .doubleLog(par, t=t)
            sse <- sum((xpred - x)^2, na.rm=TRUE)
            return(sse)
        }
        doy <- quantile(t, c(0.25, 0.75), na.rm=TRUE)
        a1 <- 0 #ok
        a2 <- 0 #ok
        # b1 <- mn #ok
        b1 <- 0
        b2 <- 0 #ok
        # c <- 0.2*max(x) # ok 
        c <- 1 # ok 
    ## very slightly smoothed spline to get reliable maximum
        tmp <- smooth.spline(x, df=0.5*length(x))
        doy.max <- which.max(tmp$y)
        # B1 <- 4/(doy.max-doy[1])        
        # # B1 <- 1/(doy.max-doy[1])
        # B2 <- 4/(doy[2]-doy.max)
        B1 <- B2 <- 0.5
        # B2 <- 1.5/(doy[2]-doy.max)
        m1 <- doy[1] + 0.5*(doy.max-doy[1])
        # # m1 <- doy[1] + 0.5*(doy.max-doy[1])
        m2 <- doy.max + 0.5*(doy[2]-doy.max)
        # m1 <- doy.max - 50
        # m2 <- doy.max + 50
        if (m1<0) m1=150
        if (m2<0 | m2>365) m2=250
        m1.bis <- doy[1]
        m2.bis  <- doy[2]  
        # q1 <- 0.5 #ok
        # q2 <- 0.5 #ok
        q1 <- 0.5 #ok
        q2 <- 0.5 #ok
        q1.bis <- 5 #ok
        q2.bis <- 5 #ok        
        # v1 <- 2 # ok
        # v2 <- 2 # ok
        v1 <- 2 # ok
        v2 <- 2 # ok
        v2.bis <- 1
        parscales <- c(0.00001, 0.00001, 0.001, 0.0001, 0.1, 0.001, 0.001, 10, 10, 10, 10, 10, 10)
        lower.b <- c(-0.01, -0.0001, -0.5, -0.001, 0.5, 0, 0, 0, 0, 0, 0, 0, 0)
        upper.b <- c(0.01, 0.0001, 0.5, 0.001, 2, 5, 5, 350, 500, 10, 10, 10, 10)
        # lower.b <- c(-0.1, -0.001, -5, -0.01, 0.5, 0, 0, 0, 0, 0, 0, 0, 0)
        # upper.b <- c(0.1, 0.001, 5, 0.01, 2, 5, 5, 350, 500, 10, 10, 10, 10)

        prior <- rbind(
            c(a1, a2, b1, b2, c, B1, B2, m1,m2, q1, q2, v1, v2),
            c(a1, a2, b1, 0.0001, 0, B1, B2, m1,m2.bis, q1, 1, v1, 4),
            c(a1, a2, b1, b2, c, B1, B2, m1.bis,m2, q1, q2, v1, v2),
            c(a1, a2, b1, b2, c, B1, B2, m1,m2.bis, q1.bis, q2, v1, v2),
            c(a1, a2, b1, b2, c, B1, B2, m1.bis,m2, q1, q2.bis, v1, v2),            
            c(a1, a2, b1, b2, c, B1, B2, m1.bis,m2, q1, q2.bis, v1, v2.bis)
            )
        # opt.l <- apply(prior, 1, optim, .error, x=x, t=t, method="BFGS", control=list(maxit=1000), hessian=hessian)   # fit from different prior values
        opt.l <- apply(prior, 1, optim, .error, x=x, t=t, method="BFGS", 
            # lower=lower.b, upper=upper.b, 
            control=list(maxit=10000
                # , parscale=parscales
                ), hessian=hessian)   # fit from different prior values
        opt.df <- cbind(cost=unlist(llply(opt.l, function(opt) opt$value)), convergence=unlist(llply(opt.l, function(opt) opt$convergence)), ldply(opt.l, function(opt) opt$par))
        pos <- which(opt.df$V5 < -2 | opt.df$V5 > 5 | opt.df$V6 < 0 | opt.df$V7 < 0 | opt.df$V8 < -0.01 | opt.df$V9 < -0.01 | opt.df$V10 < 0 | opt.df$V11 < 0)
        if (length(pos)!=0) {
            opt.df <- opt.df[-pos,]
            opt.l <- opt.l[-pos]
        }
        best <- which.min(opt.df$cost)
        if (opt.df$convergence[best] == 1) { # if maximum iterations where reached - restart from best with more iterations
        opt <- opt.l[[best]]
        opt <- optim(opt.l[[best]]$par, .error, x=x, t=t, method="BFGS", 
            # lower=lower.b, upper=upper.b, 
            control=list(maxit=50000
                # , parscale=parscales
                ), hessian=hessian)
        prior <- rbind(prior, opt$par)
        xpred <- .doubleLog(opt$par, t)
    } else if (opt.df$convergence[best] == 0) {
        opt <- opt.l[[best]]
        prior <- rbind(prior, opt$par)
        xpred <- .doubleLog(opt$par, t) ## perche questo restituisce nan?
    }
    if (opt$convergence != 0) {
        opt$par[] <- NA
        xpred <- rep(NA, length(tout))
    } else {
        xpred <- .doubleLog(opt$par, tout)
    }
    xpred <- .backnormalize(xpred, sf=sf)
    xpred.out <- zoo(xpred, order.by=t)
    names(opt$par) <- c('a1', 'a2', 'b1', 'b2', 'c', 'B1', 'B2', 'm1', 'm2', 'q1', 'q2', 'v1', 'v2') 

    if (hessian) {
        opt.new <- optim(opt$par, .error, x=x, t=t, method="BFGS", hessian=TRUE
                         ## ,              
                         ## control=list('fnscale'=-1)
           )
        .qr.solve <- function(a, b, tol = 1e-07, LAPACK=TRUE) {
            if (!is.qr(a)) 
                a <- qr(a, tol = tol, LAPACK=LAPACK)
            nc <- ncol(a$qr)
            nr <- nrow(a$qr)
            if (a$rank != min(nc, nr)) 
                stop("singular matrix 'a' in solve")
            if (missing(b)) {
                if (nc != nr) 
                    stop("only square matrices can be inverted")
                b <- diag(1, nc)
            }
            res <- qr.coef(a, b)
            res[is.na(res)] <- 0
            res        
        }
        vc <- .qr.solve(opt$hessian)
        npar <- nrow(vc)
        s2 <- opt.df$cost[best]^2 / (n - npar)
        std.errors <- sqrt(diag(vc) * s2)     # standard errors
    }

    fit.formula <- expression((a1*t + b1) + (a2*t^2 + b2*t + c)*(1/(1+q1*exp(-B1*(t-m1)))^v1 - 1/(1+q2*exp(-B2*(t-m2)))^v2))
    output <- list(predicted=xpred.out, params=opt$par, formula=fit.formula, sf=sf)
    if (hessian) output <- list(predicted = xpred.out, params = opt$par, formula = fit.formula, stdError=std.errors, sf=sf)
        return(output)
}
