plotSpatial <- function(data, param, roi.data.path, roi.name=NULL, image.path, probs=c(0.01, 0.99), plot.density=TRUE, digits=0) {
# png('/home/gian/Desktop/tmp_gian_work/smaller/VI/UD.spatial.png', width=1000, height=800, pointsize=24)
img <- readJPEG(image.path)
roi.data <- NULL
load(roi.data.path)
layout(matrix(rep(c(1,1,1,1,2),4), c(1,1,1,1,3), ncol=5, byrow=TRUE))
if (is.data.frame(data)) {
x <- data[,param]
density.data <- x
quantiles <- quantile(x, probs, na.rm=TRUE)
x[which(x < quantiles[1])] <- NA
x[which(x > quantiles[2])] <- NA
scaled <- (x-min(x,na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
pos.na <- which(is.na(scaled))
scaled[is.na(scaled)] <- 0.5
if (!is.null(roi.name)) roi.data.tmp <- roi.data[[roi.name]] else roi.data.tmp <- roi.data[[1]]
pixels.in <- roi.data.tmp$pixels.in.roi
pos.pix.roi <- which(pixels.in$pip == 1)
ratio <- dim(img)[1]/dim(img)[2]
par(mar=c(rep(1,4)))
plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ylab='', xlab='')
rasterImage(img, xleft = 0, ybottom = 0, xright = 1, 
            ytop = ratio)
grey.image <- img
grey.image[,,1][pos.pix.roi] <- 0
grey.image[,,3][pos.pix.roi] <- 0
grey.image[,,2][pos.pix.roi] <- scaled
grey.image[,,1][pos.pix.roi][pos.na] <- 1
grey.image[,,2][pos.pix.roi][pos.na] <- 1
grey.image[,,3][pos.pix.roi][pos.na] <- 1
	rasterImage(grey.image, xleft = 0, ybottom = 0, xright = 1, 
            ytop = ratio)
} else {
	## match data and roi names
	matched <- match(names(data), names(roi.data))
	roi.data.ordered <- roi.data[matched]
	roi.num <- length(roi.data.ordered)
	x <- list()
	for (a in 1:roi.num) {
		x.tmp <- data[[a]][, param]
		x[[a]] <- x.tmp
	}
	density.data <- unlist(x)
	quantiles <- quantile(unlist(x), probs, na.rm=T)
	for (a in 1:roi.num) {
		x[[a]][which(x[[a]] < quantiles[1] | x[[a]] > quantiles[2])] <- NA
	}
	min.tot <- min(unlist(x), na.rm=T)
	max.tot <- max(unlist(x), na.rm=T)
	scaled <- sapply(x, function(x) (x-min(x,na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE)))
	pos.na <- sapply(scaled, function(x) which(is.na(x)))
	scaled <- sapply(scaled, function(x) replace(x, which(is.na(x)), 0.5))
	pos.pix.roi <- sapply(roi.data.ordered, function(x) which(x$pixels.in.roi$pip==1))
	ratio <- dim(img)[1]/dim(img)[2]
	par(mar=c(rep(1,4)))
	plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ylab='', xlab='')
	rasterImage(img, xleft = 0, ybottom = 0, xright = 1, 
            ytop = ratio)
	grey.image <- img
for (a in 1:roi.num) {
	grey.image[,,1][pos.pix.roi[[a]]] <- 0
	grey.image[,,3][pos.pix.roi[[a]]] <- 0
	grey.image[,,2][pos.pix.roi[[a]]] <- scaled[[a]]
	grey.image[,,1][pos.pix.roi[[a]]][pos.na[[a]]] <- 1
	grey.image[,,2][pos.pix.roi[[a]]][pos.na[[a]]] <- 1
	grey.image[,,3][pos.pix.roi[[a]]][pos.na[[a]]] <- 1
}
	rasterImage(grey.image, xleft = 0, ybottom = 0, xright = 1, 
            ytop = ratio)
}
# par(new=TRUE, mar=c(5,4,6,4))
par(mar=c(1,1,7,4))
plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ylab='', xlab='', main=param)
y.leg <- seq(0,1, length.out=200)
x.leg <- seq(0,1, length.out=2)
z.single <- seq(0, 1, length.out=200)
z.leg <- cbind(z.single, z.single)
rgb.palette <- colorRampPalette(c('black','green'), space = "rgb")
image(x.leg, y.leg, t(z.leg), col=rgb.palette(200),
axes=F, xlab='', ylab='', add=TRUE)
at.ax <- pretty(x.leg)
at.labs <- round(seq(min(unlist(x), na.rm=T), max(unlist(x), na.rm=T), length.out=length(at.ax)), digits)
axis(4, at=at.ax, labels=at.labs)
if (plot.density) {
layout(1)
par(new=TRUE, mar=c(21,1.7,1,8.5))
plot(density(na.omit(density.data)),main='', axes=FALSE, xlab='')
abline(v=quantiles, lty=2)
axis(1, cex.axis=0.8)
box()
}
}
