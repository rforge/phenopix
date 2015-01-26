extractVIs <- function(img.path,roi.path,vi.path=NULL,roi.name=NULL,plot=TRUE, begin=NULL, spatial=FALSE, date.code) {		
  
  # require(extract.date)
  # source('/home/edo/workspace/R_script/WEBCAM/WEBCAM_v2/extract.date.R')
  
  #-----
  #img.path <- '/mnt/tresigma/cc/WEBCAM/3_ORVIELLIE/WEBCAM/2013/JPG/'
  #roi.path <- '/mnt/tresigma/cc/WEBCAM/3_ORVIELLIE/WEBCAM/2013/ROI/'
  #vi.path <- '/mnt/tresigma/cc/WEBCAM/3_ORVIELLIE/WEBCAM/2013/VI/'  
  #-----
  roi.data <- NULL
  load(paste(roi.path,'/roi.data.Rdata',sep=''))
  
  if (is.null(roi.name)) {    
    roi.name <- names(roi.data)        
  } 
  
  roi.pos <- which(names(roi.data) %in% roi.name == TRUE)
  
  files <-list.files(path=img.path)
  n_files <-length(files)  
  
  if (spatial==FALSE) {
  VI.data <- list()  
  #loop trough ROIs
  for (roi in roi.pos) {   
    temp.roi <- roi.data[[roi]]
    pos.pix.roi <- which(temp.roi$pixels.in.roi$pip == 1)
    VI.data.roi <- NULL
    #loop trough images
    for (img in seq(n_files)) {
      ## check date and begin
      temp.date <- extractDateFilename(files[img], date.code)
      if (is.na(temp.date)) stop('Something wrong in your date!')
      if (!is.null(begin)) {
      beg.date <- as.POSIXct(begin, origin='1970-01-01')
      if (beg.date >= temp.date) next()
    } else beg.date <- as.POSIXct('1970-01-01')
      print (files[img])      
      # temp.img <- readJpeg(paste(img.path,'/',files[img],sep=''))
      temp.img <- readJPEG(paste(img.path,'/',files[img],sep=''))
      temp.img[,,1] <- temp.img[,,1]*255
      temp.img[,,2] <- temp.img[,,2]*255
      temp.img[,,3] <- temp.img[,,3]*255      
      temp.r.av <- mean(temp.img[,,1][pos.pix.roi])
      temp.g.av <- mean(temp.img[,,2][pos.pix.roi])
      temp.b.av <- mean(temp.img[,,3][pos.pix.roi])
      temp.r.sd <- sd(temp.img[,,1][pos.pix.roi])
      temp.g.sd <- sd(temp.img[,,2][pos.pix.roi])
      temp.b.sd <- sd(temp.img[,,3][pos.pix.roi])
      temp.bri.av <- mean((temp.img[,,1][pos.pix.roi] + temp.img[,,2][pos.pix.roi] + temp.img[,,3][pos.pix.roi]),na.rm=TRUE)
      temp.bri.sd <- sd((temp.img[,,1][pos.pix.roi] + temp.img[,,2][pos.pix.roi] + temp.img[,,3][pos.pix.roi]),na.rm=TRUE)
      temp.gi.av <- mean(temp.img[,,2][pos.pix.roi] / (temp.img[,,1][pos.pix.roi] + temp.img[,,2][pos.pix.roi] + temp.img[,,3][pos.pix.roi]),na.rm=TRUE)
      temp.gi.sd <- sd(temp.img[,,2][pos.pix.roi] / (temp.img[,,1][pos.pix.roi] + temp.img[,,2][pos.pix.roi] + temp.img[,,3][pos.pix.roi]),na.rm=TRUE)
      temp.gei.av <- mean( (2*temp.img[,,2][pos.pix.roi]) - temp.img[,,1][pos.pix.roi] - temp.img[,,3][pos.pix.roi] ,na.rm=TRUE)
      temp.gei.sd <- sd( (2*temp.img[,,2][pos.pix.roi]) - temp.img[,,1][pos.pix.roi] - temp.img[,,3][pos.pix.roi] ,na.rm=TRUE)
      temp.ri.av <- mean(temp.img[,,1][pos.pix.roi] / (temp.img[,,1][pos.pix.roi] + temp.img[,,2][pos.pix.roi] + temp.img[,,3][pos.pix.roi]),na.rm=TRUE)
      temp.ri.sd <- sd(temp.img[,,1][pos.pix.roi] / (temp.img[,,1][pos.pix.roi] + temp.img[,,2][pos.pix.roi] + temp.img[,,3][pos.pix.roi]),na.rm=TRUE)
      temp.bi.av <- mean(temp.img[,,3][pos.pix.roi] / (temp.img[,,1][pos.pix.roi] + temp.img[,,2][pos.pix.roi] + temp.img[,,3][pos.pix.roi]),na.rm=TRUE)
      temp.bi.sd <- sd(temp.img[,,3][pos.pix.roi] / (temp.img[,,1][pos.pix.roi] + temp.img[,,2][pos.pix.roi] + temp.img[,,3][pos.pix.roi]),na.rm=TRUE) 
      temp.doy <- as.numeric(format(temp.date,format="%j"))
      temp.VI <- data.frame(date = temp.date, doy = temp.doy, r.av = temp.r.av, g.av = temp.g.av, b.av = temp.b.av, r.sd = temp.r.sd, g.sd = temp.g.sd, b.sd = temp.b.sd, bri.av = temp.bri.av, bri.sd = temp.bri.sd,
                            gi.av = temp.gi.av, gi.sd = temp.gi.sd, gei.av = temp.gei.av, gei.sd = temp.gei.sd, ri.av = temp.ri.av, ri.sd = temp.ri.sd, bi.av = temp.bi.av, bi.sd = temp.bi.sd)      
      VI.data.roi <- rbind(VI.data.roi,temp.VI)     
    } #endfor loop images  
  end <- max(VI.data.roi$date, na.rm=TRUE)  
  if (end < beg.date) stop('Your final date is later than last record in your timeseries')
    end <- trunc(end, 'day')
    VI.data[[roi]] <- VI.data.roi         
    if (plot == TRUE) {
if (is.null(begin)) {
      png(filename=paste(vi.path,roi.name[roi],'_roi_VI_plot.png',sep=''), width=800, height=5*400, pointsize=30)  
         } else {
      png(filename=paste(vi.path,begin, '_', end, '_',roi.name[roi],'_roi_VI_plot.png',sep=''), width=800, height=5*400, pointsize=30)           
         }
        par(mfrow=c(5,1))
        par(mar=c(3,4,2,0.5))
        plot(VI.data.roi$date,VI.data.roi$r.av,col='red',pch=20,xlab='',ylab='R-G-B',main=paste('ROI: ',roi.name[roi],sep=''))
        points(VI.data.roi$date,VI.data.roi$g.av,col='green',pch=20)  
        points(VI.data.roi$date,VI.data.roi$b.av,col='blue',pch=20)
        par(mar=c(3,4,0.5,0.5))
        plot(VI.data.roi$date,VI.data.roi$ri.av,col='red',pch=20,xlab='',ylab='RI')
        par(mar=c(3,4,0.5,0.5))  
        plot(VI.data.roi$date,VI.data.roi$gi.av,col='green',pch=20,xlab='',ylab='GI')
        par(mar=c(3,4,0.5,0.5))  
        plot(VI.data.roi$date,VI.data.roi$bi.av,col='blue',pch=20,xlab='',ylab='BI')
        par(mar=c(4,4,0.5,0.5)) 
        plot(VI.data.roi$date,VI.data.roi$bri.av,col='grey',pch=20,xlab='doy',ylab='BRI')
        
      dev.off()
    }   
  } #endfor loop rois
  
  names(VI.data) <- roi.name
  if (is.null(begin)) {
  save(VI.data,file=paste(vi.path,'VI.data.Rdata',sep=''))
} else {
  save(VI.data,file=paste(vi.path,begin,'_', end, '_', 'VI.data.Rdata',sep=''))  
}
} else {## if spatial == TRUE
VI.data <- list()
 for (roi in roi.pos) {    
    temp.roi <- roi.data[[roi]]
    pos.pix.roi <- which(temp.roi$pixels.in.roi$pip == 1)
    VI.data.roi <- list()
    #loop trough images
    for (img in seq(n_files)) {
      ## check date and begin
      temp.date <- extractDateFilename(files[img], date.code)
      if (!is.null(begin)) {
      beg.date <- as.POSIXct(begin, origin='1970-01-01')
      if (beg.date >= temp.date) next()
    }
      print (files[img])
      # temp.img <- readJpeg(paste(img.path,'/',files[img],sep=''))
      temp.img <- readJPEG(paste(img.path,'/',files[img],sep=''))
      temp.img[,,1] <- temp.img[,,1]*255
      temp.img[,,2] <- temp.img[,,2]*255
      temp.img[,,3] <- temp.img[,,3]*255      
      all.reds <- temp.img[,,1][pos.pix.roi] 
      all.greens <- temp.img[,,2][pos.pix.roi] 
      all.blue <- temp.img[,,3][pos.pix.roi] 
      pixel.df <- data.frame(red=all.reds, green=all.greens, blue=all.blue)
      VI.data.roi[[img]] <- pixel.df
      names(VI.data.roi)[img] <- temp.date
      #points(poligono$colpos,ratio-poligono$rowpos,pch='.')
}
### remove unprocessed data if 
null.pos <- which(lapply(VI.data.roi, is.null)==TRUE)
if (length(null.pos)!=0) VI.data.roi <- VI.data.roi[-null.pos]
dates <- as.POSIXct(as.numeric(names(VI.data.roi)), origin='1970-01-01')
names(VI.data.roi) <- dates
end <- max(dates, na.rm=TRUE)  
if (!is.null(begin)) if (end < beg.date) stop('Your final date is later than last record in your timeseries')
end <- trunc(end, 'day')
VI.data[[roi]] <- VI.data.roi
}  ## end roi loop
names(VI.data) <- roi.name
  if (is.null(begin)) {
  save(VI.data,file=paste(vi.path,'VI.data.spatial.Rdata',sep=''))
} else {
  save(VI.data,file=paste(vi.path,begin,'_', end, '_', 'VI.data.spatial.Rdata',sep=''))  
}
}
invisible(VI.data)	 
}
