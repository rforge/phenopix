plotClusters <- function(input, roi.data.path, image.path, upadj = NULL, 
  rightadj = NULL, legend=TRUE) {
  .plotImage <- function(image, add=FALSE, ...) {
  ncols <- ncol(image)
  nrows <- nrow(image)
  if (!add) {
    plot(0, type = "n", xlim = c(0, ncols), ylim = c(0, nrows), 
      ...)
  }
  rasterImage(image, xleft = 0, ybottom = 0, xright = ncols, 
    ytop = nrows)
}
  data <- input$clusters
  param <- 'clusters'
  trajectories <- input$curves
  img <- readJPEG(image.path)
  roi.data <- NULL
  load(roi.data.path)
  x <- data[, param]
  colors <- palette()[1:length(table(x))]
  rgb.colors <- col2rgb(colors)
  rgb.colors <- rgb.colors/max(rgb.colors)
      # zeropos <- which(rgb.colors==0)
      # onepos <- which(rgb.colors==1)
      # rgb.colors[zeropos] <- 1
      # rgb.colors[onepos] <- 0
  rgb.colors <- as.data.frame(rgb.colors)
  names(rgb.colors) <- c(1:length(colors))
  roi.data.tmp <- roi.data[[1]]
  pixels.in <- roi.data.tmp$pixels.in.roi
  pos.pix.roi <- which(pixels.in$pip == 1)
  ratio <- dim(img)[1]/dim(img)[2]
  par(mar = c(rep(1, 4)))
  .plotImage(img, axes=FALSE)
  # plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, 
  #  ylab = "", xlab = "")
  # rasterImage(img, xleft = 0, ybottom = 0, xright = 1, 
  #   ytop = ratio)
  rgb.data.frame <- data.frame(matrix(ncol=3, nrow=length(x)))
  for (a in 1:length(colors)) {
    pos.tmp <- which(x==a)
    rgb.data.frame[pos.tmp, 1] <- rgb.colors[1,a]
    rgb.data.frame[pos.tmp, 2] <- rgb.colors[2,a]
    rgb.data.frame[pos.tmp, 3] <- rgb.colors[3,a]
  }
  grey.image <- img
  pos.na <- which(is.na(x)==TRUE)
  if (length(pos.na)!=0) {
    grey.image[,,1][pos.pix.roi][-pos.na] <- rgb.data.frame[-pos.na,1]
    grey.image[,,2][pos.pix.roi][-pos.na] <- rgb.data.frame[-pos.na,2]
    grey.image[,,3][pos.pix.roi][-pos.na] <- rgb.data.frame[-pos.na,3]
  } else {
    grey.image[,,1][pos.pix.roi] <- rgb.data.frame[,1]
    grey.image[,,2][pos.pix.roi] <- rgb.data.frame[,2]
    grey.image[,,3][pos.pix.roi] <- rgb.data.frame[,3]
  }
  .plotImage(grey.image, add=TRUE)  
  legend.names <- paste0('cluster',1:length(colors))
  if (legend) legend('topleft', col=colors, legend=legend.names, lty=1, lwd=2, bg='white', inset=0.05)
}
