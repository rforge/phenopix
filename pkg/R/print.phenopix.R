print.phenopix  <- function(x, ...) {
    attr.retr <- attributes(x)
    cat('\n #### phenopix time series processing ####\n')
    cat(paste0('\nFITTING: ', toupper(attr.retr$fit), '\n'))
    cat('\nPREDICTED VALUES:\n')
    print(summary(x$fit$fit$predicted))  
    cat('\nFITTING EQUATION:\n')
    print(x$fit$fit$formula)  
   cat('\nFITTING PARAMETERS:\n')
    print(x$fit$fit$params)  
    if (attr.retr$uncert) {
    cat(paste0('\nTHRESHOLDS: ', toupper(attr.retr$threshold),'  ENVELOPE:', toupper(attr.retr$envelope),'\n'))
} else cat(paste0('\nTHRESHOLDS: ', toupper(attr.retr$threshold),'\n'))
    print(x$metrics)  
    cat(paste0('\nUNCERTAINTY: ', toupper(attr.retr$uncert), '\n N of replications = ', attr.retr$nrep), '\n')
}