spatialGreen <- function(spatial.list, fit, threshold, filters='default',parallel=TRUE, save=FALSE, path=NULL, assign=TRUE) {
    if (save==FALSE & assign ==FALSE) stop('Arguments save and assign cannot be both FALSE!')
    time <- as.POSIXct(names(spatial.list))
    npixels <- dim(spatial.list[[1]])[1]
    # fitting.list <- NULL
    # for (a in 1:npixels) {
    if (parallel) {
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    fitting.list <- foreach(a=1:npixels, .packages=c('phenopix')) %dopar% {
    #   require(zoo)
    single.pixel <- NULL
    for (i in 1:length(spatial.list)) {
        single.triplet <- spatial.list[[i]][a,]
        single.pixel <- rbind(single.pixel, single.triplet)
    }       
    single.pixel$time <- time
    if (filters=='default') {
        filtered.tmp <- phenopix::autoFilter(single.pixel, raw.dn=TRUE, dn=1:3, plot=FALSE)$max.filtered
    } else {
        filtered.tmp <- phenopix::autoFilter(single.pixel, raw.dn=TRUE, dn=1:3, plot=FALSE, filter=filters)
        if (length(filters)!=1) {
    ncols <- dim(filtered.tmp)[2]           
    filtered.tmp <- filtered.tmp[,ncols]            
        }  
    }
    fitted.tmp <- try(phenopix::greenProcess(na.approx(filtered.tmp), fit, threshold, plot=FALSE))
    if (save) {
        filename <- paste0(path, '/fitted.tmp', a, '.Rdata')
        save(fitted.tmp, file=filename)
        if (!assign) fitted.tmp <- NULL
    }
    # fitting.list[[a]] <- fitted.tmp
    print(round(a/npixels*100,2))
#     return(fitted.tmp)
}
stopCluster(cl)
## if a fit is performed, there is a fit dataframe
# beg <- grep('fit', names(fitting.list))
# ## 4 elements in the phenopix list
# end <- beg+3
# ## beg sequence if all fits work
# complete.beg <- seq(1, by=4, length.out=npixels)
# matched <- macth(complete.beg)
# loop.code.bol <- which(complete.beg %in% beg ==FALSE)
# loop.code.complete <- seq(1, length(loop.code.bol))
# loop.code <- ifelse(loop.code.bol, loop.code.complete, NA)
# beg.code <- 1:length(loop.code)
# final.container <- list()
# for (a in 1:length(beg)) {
#     list.pos <- 1
#     act.beg <- beg[a]
#     act.end <- end[a]    
#     act.group <- fitting.list[act.beg:act.end]
#     ## set structure and attributes to output for summary, print, etc
#     info <- list(fit=fit, threshold=threshold, uncert=FALSE, envelope=NULL, 
#     quantiles=NULL, nrep=0)
# info[7] <- attributes(act.group)
# names(info)[7] <- 'names'
# attributes(act.group) <- info
# class(act.group) <- 'phenopix'
# final.container[[list.pos]] <- act.group
# }
} else {
    fitting.list <- NULL
    #   require(zoo)
    for (a in 1:npixels) {
    single.pixel <- NULL
    for (i in 1:length(spatial.list)) {
        single.triplet <- spatial.list[[i]][a,]
        single.pixel <- rbind(single.pixel, single.triplet)
    }       
    single.pixel$time <- time
    if (filters=='default') {
        filtered.tmp <- phenopix::autoFilter(single.pixel, raw.dn=TRUE, dn=1:3, plot=FALSE)$max.filtered
    } else {
        filtered.tmp <- phenopix::autoFilter(single.pixel, raw.dn=TRUE, dn=1:3, plot=FALSE, filter=filters)
        if (length(filters)!=1) {
    ncols <- dim(filtered.tmp)[2]           
    filtered.tmp <- filtered.tmp[,ncols]            
        }  
    }
    fitted.tmp <- try(phenopix::greenProcess(na.approx(filtered.tmp), fit, threshold, plot=FALSE))
    fitting.list[[a]] <- fitted.tmp
}
# final.container <- fitting.list
}
if (!assign) return(fitting.list)
}
