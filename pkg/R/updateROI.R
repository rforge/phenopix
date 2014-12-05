updateROI <- function(old.roi.path, new.path.img.ref, new.roi.path) {
    file <- list.files(path = new.path.img.ref, pattern = ".jpg")
    img <- readJPEG(paste(new.path.img.ref, file, sep = ""))
    img.converted <- img
    img.converted[, , 1] <- img[, , 1] * 255
    img.converted[, , 2] <- img[, , 2] * 255
    img.converted[, , 3] <- img[, , 3] * 255
    ratio <- dim(img)[1]/dim(img)[2]
    load(paste0(old.roi.path, 'roi.data.Rdata'))
    old.roi.data <- roi.data
    roi.names <- names(roi.data)
    roi.data <- NULL
    nroi <- length(old.roi.data)
    for (i in seq(as.numeric(nroi))) {
        vertices <- old.roi.data[[i]]$vertices
        coordinates <- data.frame(rowpos = ratio - vertices$y, 
            colpos = vertices$x)
        image.array <- expand.grid(rowpos = seq(1:nrow(img))/(nrow(img)/ratio), 
            colpos = seq(1:ncol(img))/ncol(img))
        pixels.in.roi <- pnt.in.poly(image.array, coordinates)
        ## dev.print(jpeg, file = paste(path_ROIs, "/ROI", i, "_", 
        ##     roi.names[i], ".jpg", sep = ""), width = 1024, height = 1024)
        ## dev.off()
        out <- list(pixels.in.roi, vertices)
        names(out) <- c("pixels.in.roi", "vertices")
        roi.data[[i]] <- out
    }
    names(roi.data) <- roi.names
    save(roi.data, file = paste(new.roi.path, 'roi.data.Rdata', 
        sep = ""))
    return(roi.data)
}
