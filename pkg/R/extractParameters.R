extractParameters <- function(path, list=NULL, update=NULL) {
	if (is.null(list) & is.null(path)) stop('Provide a path to folder where single pixel Rdata are stored \n or a list \n as in output from spatialGreen') 
if (!is.null(list)) {
	### look for first fitted element
	first.element <- list[[1]]
	while (class(first.element)=='try-error') {
		i <- 1
		first.element <- list[[i]]
		i <- i + 1
	}
if (!is.null(update)) {
	updated <- update(first.element, update, plot=FALSE)
		metrics.tmp <- extract(first.element, 'metrics')
	} else metrics.tmp <- extract(first.element, 'metrics')
	params.tmp <- extract(first.element, 'curve.params')
	metrics.length <- length(metrics.tmp)
	params.length <- length(params.tmp)
	gl.length <- metrics.length + params.length + 1
	df.exit <- data.frame(matrix(ncol=gl.length, nrow=length(list)))
	names(df.exit) <- c(names(metrics.tmp), names(params.tmp), 'RMSE')	
	for (a in 1:length(list)) {
		act.element <- list[[a]]
	if (class(act.element)=='try-error') {
		exit.row <- rep(NA, gl.length)
	} else {
	if (!is.null(update)) {
		updated <- update(act.element, update, plot=FALSE)
		metrics.tmp <- extract(updated, 'metrics')
	} else metrics.tmp <- extract(act.element, 'metrics')
	params.tmp <- extract(act.element, what='curve.params')
	fit.tmp <- try(lm(extract(act.element, what='fitted') ~ extract(act.element, what='data')))
    RMSE <- ifelse(class(fit.tmp)=='try-error', NA, summary(fit.tmp)$sigma)
	exit.row <- c(metrics.tmp, params.tmp, RMSE=RMSE) 	
}
	df.exit[a, ] <- exit.row
	print(a)
}

} else {
	### if we are here, it means we have stored tmp fittings in a folder
	complete.files <- list.files(path, full.names=TRUE)
	files <- list.files(path)
	fitted.tmp <- NULL
	load(complete.files[1])
	while (class(fitted.tmp)=='try-error') {
		i <- 1
		load(complete.files[i])
		i <- i + 1
	}
	if (!is.null(update)) {
		updated <- update(fitted.tmp, update, plot=FALSE)
		metrics.tmp <- extract(updated, 'metrics')
	} else metrics.tmp <- extract(fitted.tmp, 'metrics')
	params.tmp <- extract(fitted.tmp, 'curve.params')
	metrics.length <- length(metrics.tmp)
	params.length <- length(params.tmp)
	gl.length <- metrics.length + params.length + 1
	df.exit <- data.frame(matrix(ncol=gl.length, nrow=length(files)))
	names(df.exit) <- c(names(metrics.tmp), names(params.tmp), 'RMSE')
for (a in 1:length(files)) {
	act.file <- complete.files[a]
	act.file.name <- files[a]
	act.index <- as.numeric(substr(act.file.name, 11, nchar(act.file.name)-6))
	load(act.file)
	if (class(fitted.tmp)=='try-error') {
		exit.row <- rep(NA, gl.length)
	} else {
	if (!is.null(update)) {
		updated <- update(fitted.tmp, update, plot=FALSE)
		metrics.tmp <- extract(updated, 'metrics')
	} else metrics.tmp <- extract(fitted.tmp, 'metrics')
	params.tmp <- extract(fitted.tmp, what='curve.params')
	fit.tmp <- try(lm(extract(fitted.tmp, what='fitted') ~ extract(fitted.tmp, what='data')))
    RMSE <- ifelse(class(fit.tmp)=='try-error', NA, summary(fit.tmp)$sigma)
	exit.row <- c(metrics.tmp, params.tmp, RMSE=RMSE) 	
}
	df.exit[act.index, ] <- exit.row
	print(act.index)
}
}
return(df.exit)
}
