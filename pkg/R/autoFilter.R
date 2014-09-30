autoFilter <-
function(data, dn, raw.dn=TRUE, brt=NULL, na.fill=TRUE, 
    filter=c('night','spline', 'max'), filter.options=NULL, 
    plot=TRUE, ...) {
    ## define internal functions
.night.filter <- function(data, act.opts, name='gcc',...) {
    pos.dark <- which(data$gcc<act.opts$threshold)
    filtered <- data[,name]
    if (length(pos.dark)!=0) {
        filtered[pos.dark] <- NA
    }
    return(filtered)
}

## a filter for clouds based on a threshold on blue channel. A very aggressive filter for very bad images, 
## never useful on phenocam images

.blue.filter <-function(data, act.opts, name='gcc', ...) {
    threshold <- act.opts$threshold
    ## compute daily standard deviation
    tmp.daily.sd <- aggregate(data$bcc, by=list(data$doy), FUN=sd, na.rm=T)
    ## compute daily mean
    tmp.daily.mean <- aggregate(data$bcc, by=list(data$doy), FUN=mean, na.rm=T)
    ##set a threshold on 5% of daily standard deviation
    sd_med_BI_all_season<-quantile(tmp.daily.sd[,2],probs=threshold,na.rm=TRUE,type=3)
    ##if images are daily, daily sd is NA, so filter is set at 0.5
    if (is.na(sd_med_BI_all_season)) {sd_med_BI_all_season<-0.5}
    ##be larger when sd_med_BI_all_season is too small
    if (sd_med_BI_all_season==0) {sd_med_BI_all_season<-quantile(tmp.daily.sd[,2],probs=threshold*2,na.rm=TRUE,type=3)}
    ## set the envelope between actual daily mean and 5% standard deviation
    lower.env <- tmp.daily.mean[,2]-sd_med_BI_all_season
    upper.env <- tmp.daily.mean[,2]+sd_med_BI_all_season
    ##assign flag according to the condition
    data$flag <- NA
    for (i in tmp.daily.sd[,1]) {
        act.doy <- i
        act.lower <- lower.env[tmp.daily.sd[,1]==act.doy]
        act.upper <- upper.env[tmp.daily.sd[,1]==act.doy]
        act.subset <- data[data$doy==act.doy, ]
        act.subset$flag <- ifelse(act.subset$bcc>act.lower & act.subset$bcc<act.upper, 'keep', 'discard')
        data$flag[data$doy==act.doy] <- act.subset$flag
    }
    ## clean filtered data
    filtered <- data[,name]
    filtered[data$flag=='discard'] <- NA
    ## return data.frame
    ## final.data.frame <- data.frame(time=data$time, doy=data$doy, raw=data[,name], blue.filter=filtered)
    return(filtered)
}

## a filter based on gcc as implemented in Papale et al., 2006 (Biogeosciences)
.mad.filter <- function(data, act.opts, name='gcc', ...) {
    z <- act.opts$z
    data$mad <- NA
    tser=data[,name]
    i=length(tser)-1
    ii=length(tser)-2
    iii=length(tser)
    ## array of the differences
    d=(tser[2:i]-tser[1:ii])-(tser[3:iii]-tser[2:i])
    d=c(d[1],d,d[length(d)])
    Md=median(d)
    mad_data=median(abs(d-Md))
    upper=(Md+(z*mad_data/0.6745))
    lower=(Md-(z*mad_data/0.6745))
    data$flag <- ifelse(d>lower & d<upper, 'keep', 'discard')
    filtered <- data[,name]
    filtered[data$flag=='discard'] <- NA
    ## return data.frame
    return(filtered)
}

## 90th quantile filter on a 3 days moving window
.max.filter <- function(data, name='gcc', act.opts, ...) {
    w <- act.opts$w
    .max.fun <- function(x) {
    quantile(x, 0.9, na.rm=T)
    }
    computed.frequency <- median(diff(as.numeric(data$time)), na.rm=T)
    data.per.day <- median(aggregate(data[,1], by=list(as.numeric(as.character(data$doy))), FUN=length)[,2], na.rm=T)
    # days.freq <- computed.frequency/60/60/24
    # one.day.slot <- 1/days.freq
    true.window <- data.per.day*w
    new.max <- rollapply(data[,name], FUN=.max.fun, width=true.window, fill='extend')
    # doy.seq <- as.numeric(unique(data$doy))
    # ## a doy sequence of disired window
    # index.beg <- seq(range(doy.seq)[1], range(doy.seq)[2]-(w-1))
    # index.end <- seq(range(doy.seq)[1]+(w-1), range(doy.seq)[2])
    # VImax<-data[,name]
    # rolled <- VImax
    # for (i in 1:length(index.beg)) {
    #     act.beg <- index.beg[i]
    #     act.end <- index.end[i]
    #     days.in <- act.beg:act.end
    #     ## if first window, set it in either first and second day
    #     if (i==1) {day.to.set <- days.in[1:(w-1)]}
    #     ## if last window, set it in either second and third (last) day
    #     if (i==length(index.beg)) {day.to.set <- days.in[2:w]}
    #     ## else set it in second day
    #     if (i!=1 & i!=length(index.beg)) {day.to.set <- days.in[2:(w-1)]}
    #     pos.to.keep <- which(as.numeric(data$doy) %in% days.in==TRUE)
    #     pos.to.set <- which(as.numeric(data$doy) %in% day.to.set==TRUE)
    #     max.quant <- quantile(na.omit(VImax[pos.to.keep]),probs=(0.9))
    #     rolled[pos.to.keep] <- max.quant
    #     pos.na <- which(is.na(VImax[pos.to.keep])==TRUE)
    #     rolled[pos.to.keep][pos.na] <- NA
    # }
    ## final.data.frame <- data.frame(time=data$time, doy=data$doy, raw=data[,name], max.filter=rolled)
    # return(rolled)
 return(new.max)   
}

## spline filter as used in Migliavacca et al. 2011 (Agricultural and Forest Meteorology)
.spline.filter <- function(data, act.opts, name='gcc', ...) {
    stdup <- act.opts$stdup
    stddown <- act.opts$stddown
    loop_spline <- act.opts$loop_spline
    if(length(data$doy) >= 100) {df_set<-10}
    if(length(data$doy) < 100) {df_set<-5}
    data.in.the.loop <- data[!is.na(data[,name]),]
    for(iii in 1:loop_spline){
        if (length(unique(data.in.the.loop$time))>=10) {
            splineVI<-predict(smooth.spline(data.in.the.loop$time, data.in.the.loop[,name], df=length(unique(data.in.the.loop$doy))/df_set))
            d1spline <- predict(smooth.spline(data.in.the.loop$time, data.in.the.loop[,name], df=length(unique(data.in.the.loop$doy))/df_set), d=1)
            abs.d1 <- abs(d1spline$y/max(d1spline$y))*1.5
            ## check data removed from spline
            time.tmp <- as.POSIXct(splineVI$x, origin='1970-01-01')
            pos.match <- which(data.in.the.loop$time %in% time.tmp ==TRUE)
            filled.splineVI <- rep(NA, length(data.in.the.loop[,1]))
            weight.vect <- rep(NA, length(data.in.the.loop[,1]))
            filled.splineVI[pos.match] <- splineVI$y
            weight.vect[pos.match] <- abs.d1
            ## splineVI<-splineVI$y
            res_splineVI<-data.in.the.loop[,name]-filled.splineVI
            outliersVI = which(res_splineVI<(mean(res_splineVI)-(stddown+weight.vect)*sd(res_splineVI)) |
                res_splineVI>(mean(res_splineVI)+(stdup+weight.vect)*sd(res_splineVI)))
            ## outliersVI = c(outliersVI, which(res_splineVI>(mean(res_splineVI)+stdup*sd(res_splineVI))))
            badones = outliersVI
            if (length(badones)==0) {data.in.the.loop<-data.in.the.loop} else {data.in.the.loop<-data.in.the.loop[-badones,]}
                                        #print(paste('spline filtering: ',iii,' there are',length(badones),'outliers'))
                                        #print(iii)
        }
    }
    ## return a complete data.frame
    data_merged<-merge(data,data.in.the.loop[,c('time', name)],by='time',all.x=TRUE)
    name.merged <- paste(name, '.y', sep='')
    filtered <- data_merged[,name.merged]
    ## final.data.frame <- data.frame(time=data$time, doy=data$doy, raw=data[,name], spline.filter=data_merged[,name.merged])
    return(filtered)
}

    if (is.null(filter.options)) {filter.options <- list(night.filter=list(threshold=0.2),
                      blue.filter=list(threshold=0.05),
                      mad.filter=list(z=15),
                      max.filter=list(w=3),
                      spline.filter=list(stdup=4, stddown=4, loop_spline=20))
                          }
    classes <- NULL
    for (a in 1:length(data)) {
        tmp <- class(data[,a])[1]
        classes <- c(classes, tmp)
    }
    pos.POSIX <- which(classes=='POSIXct')
    if (length(pos.POSIX)==0) stop("Missing POSIX vector in your data")
    if (length(pos.POSIX)>1) warning("Multiple POSIX vectors: only the first one is used")
    time <- data[,pos.POSIX[1]]
    ## check and erase duplicated timestamp
    if (length(which(duplicated(time)))!=0) {
        pos.dupl <- which(duplicated(time))
        time <- time[-pos.dupl]
        data <- data[-pos.dupl,]
        }
    time.range <- length(unique(format(time, '%Y')))
    if (time.range>1) warning('Filtering is optimized for a single year of data: use results with caution')
    doy <- format(time, "%j")
    ## retrieve data vectors if raw.dn = TRUE
    if (raw.dn==TRUE) {
    rgb.colors <- data[,dn]
    brt <- apply(rgb.colors, MARGIN=1, FUN='sum', na.rm=T)
    ## build color indices
    rgb.indices <- rgb.colors/brt
    } else {
        if (is.null(brt)) warning('Provide column name for brightness')
        brt <- data[,brt]
        rgb.indeces <- data[,dn]
        }
    names(rgb.indices) <- c('rcc', 'gcc', 'bcc')
    new.data <- data.frame(time, doy, rgb.indices, brt)
    ##recursive filtering
    filter.names <- paste(filter, '.filtered', sep='')
    name.to.filter <- c('gcc', paste(filter,'.filtered', sep=''))
    filtered.data <- new.data
    for (a in 1:length(filter)) {
        act.filter <- filter[a]
        act.name <- filter.names[a]
        act.name.to.filter <- name.to.filter[a]
        if (act.filter=='night') {
            act.fun <- .night.filter
            act.opts <- filter.options$night.filter
        }
        if (act.filter=='max') {
            act.fun <- .max.filter
            act.opts <- filter.options$max.filter
        }
        if (act.filter=='spline') {
            act.fun <- .spline.filter
            act.opts <- filter.options$spline.filter
        }
        if (act.filter=='blue') {
            act.fun <- .blue.filter
            act.opts <- filter.options$blue.filter
        }
        if (act.filter=='mad') {
            act.fun <- .mad.filter
            act.opts <- filter.options$mad.filter
        }
        filtered <- act.fun(filtered.data, act.opts, name=act.name.to.filter)
        filtered <- data.frame(filtered)
        names(filtered) <- act.name
        filtered.data <- data.frame(filtered.data, filtered)
        ## colname <- paste(act.filter, '.filtered', sep='')
    }
    ## daily aggregation by doy, return median
    pos.time <- which(names(filtered.data)=='time' | names(filtered.data)=='doy')
    daily.agg <- aggregate(filtered.data[,-pos.time], by=list(filtered.data$doy), FUN='median', na.rm=T)
    names(daily.agg)[1] <- 'doy'    
    ## end of the filtering loop
    if (plot==TRUE) {
        ##define palette
        act.palette <- palette()[1:(length(filter)+1)]
        legend.names <- c('raw', filter)
        names.match <- which(names(filtered.data)%in%paste(filter, '.filtered', sep=''))
        ylims <- range(c(filtered.data$gcc, filtered.data[,names.match]), na.rm=T)
        with(filtered.data, plot(time, gcc, col=act.palette[1], ylim=ylims, pch=16))
        if (length(filter)==1) points(filtered.data$time, filtered.data[,names.match], col=act.palette[2], pch=16) else {
        for (i in 1:length(filter)) {
            points(filtered.data$time, filtered.data[,names.match][,i], col=act.palette[i+1], pch=16)
        }
    }
        legend('bottomright', col=act.palette, legend=legend.names, pch=16, bty='n')
    }
    doys <- as.numeric(as.character(daily.agg$doy))
    pos.days <- which(names(daily.agg)=='doy')
    reduced <- daily.agg[,-pos.days]
    ##aggregate daily
    if (na.fill) to.return <- na.approx(zoo(reduced, order.by=doys), maxgap=10) else {
        to.return <- zoo(reduced, order.by=doys)
    }
        return(to.return)

}
