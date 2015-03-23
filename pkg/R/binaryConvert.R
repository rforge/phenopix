binaryConvert <- function(ipath, opath) {
	all.jpg <- list.files(ipath)
	all.jpg.f <- list.files(ipath, full.names=TRUE)
	for (a in 1:length(all.jpg)) {
		img<-readJPEG(all.jpg.f[a])
		grey.image <- 0.2126*img[,,1] + 0.7152*img[,,2] + 0.0722*img[,,3]
		binary <- round(grey.image, 0)
		rev.binary <- ifelse(binary==1, 0, 1)
		out <- paste0(opath, all.jpg[a])
		writeJPEG(rev.binary, out)
		print(all.jpg[a])
	}
}