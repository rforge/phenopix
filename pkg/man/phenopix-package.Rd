\name{phenopix-package}
\alias{phenopix-package}
\alias{phenopix}
\docType{package}
\title{
A package to process images of a vegetation cover   
}
\description{
The package provides functions to process digital images, depict greennes index 
trajectories and extract relevant phenological stages. 
}
\details{
\tabular{ll}{
Package: \tab phenopix\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-08-21\cr
License: \tab GPL-2\cr
}

The package provides functions to process digital images, depict greennes index 
trajectories and extract relevant phenological stages. The first step of the work 
is to set a region of interest on the images. The function \code{DrawROI} does the work. 
More than one ROI can be specified. 

Second step is extracting information from the ROI(s). The function \code{extractVIs}
computes several vegetation indexes on image pixels falling within the ROI(s). 
The function works recursively within a folder so that all images can be processed 
and a time series of the computed indexes is extracted. A specific function 
\code{extractDateFilename} retrives a timestamp from the filename of the images. 
Time series of green, red and blue chromatic coordinates and brightenss are returned 
for each ROI of the image. 

Third step is data filtering. The function \code{autoFilter} provides several filtering 
methods that can be used one at a time or in sequence.

Fourth step is fitting a curve to the data. Five methods have been included in the package. 
The function \code{SplineFit} fits a smoothed cubic spline to the data. The remaining four 
methods are based on the optimization of different double logistic equations retrieved 
from the recent literature. An uncertainty calculation is provided, based on the residuals between
observed and predicted values. The variability in the residuals is used to generate random noise, 
that is then added to raw data in a bootstrap procedure. From this procedure an ensemble of equation 
parameters and/or of curves are generated. 

Fifth step is the extraction of phenological thresholds. Five methods have been 
included to extract phenological dates. One is based on thresholds on the spline 
curve and one on breakpoints analysis, whereas other methods work on inflection points 
of the derivatives in various ways.  
Uncertainty on curve fitting is extended to threshold extraction, so that also 
for this step uncertainty is easily estimated.

The package offers a variety of fittings and thresholdings so to be as flexible 
as possible in order to apply to very different boreal (sofar) ecosystems, ranging 
from high latitude/altitude grasslands to tropical forests. 
Ecosystems that show multiple seasonal peaks can be splitted in subseason with 
the \code{CutSeason} function and processed separately. 
Dedicated plotting functions provide an easy way to look at fitting and thresholding 
with annotated graphs. 
The package is beeing tested on the PHENOCAM dataset (http://phenocam.sr.unh.edu/webcam/), 
and constantly debugged. 

In the example section the user can find a walk-through to understand main functions, 
from the filtering step forward. ROI selection and index calculations are excluded 
from the example because they require the use of locator from the user. 
}

\author{
Gianluca Filippa, Edoardo Cremonese, Mirco Migliavacca, ... 

Maintainer: Gianluca Filippa <gian.filippa@gmail.com>
}
\references{
Gu L, Post WM, Baldocchi D, Black TA, Suyker AE, Verma SB, Vesala T, 
Wofsy SC. (2009) Characterizing the Seasonal Dynamics of Plant Community 
Photosynthesis Across a Range of Vegetation Types. In: Phenology of Ecosystem 
Processes (Ed: Noormets A, Springer New York), pp 35-58. 

Klosterman ST, Hufkens K, Gray JM, Melaas E, Sonnentag O, Lavine I, Mitchell L, 
Norman R, Friedl MA, Richardson A D (2014) Evaluating remote sensing of deciduous 
forest phenology at multiple spatial scales using PhenoCam imagery, Biogeosciences, 
11, 4305-4320, doi:10.5194/bg-11-4305-2014.

Migliavacca et al 2011

Papale et al.

Sonnentag et al. 


Zhang X, Friedl MA, Schaaf CB, Strahler AH, Hodges JCF, Gao F, Reed BC, Huete A (2003) 
Monitoring vegetation phenology using MODIS, Remote Sens. Environ., 84, 471-475.

}
\keyword{ package }
\seealso{
greenbrown.r-forge.r-project.org
}
\examples{

## step 3: filter data
data(bartlett2009)
## with raw.dn ==TRUE and column position for chromatic coordinates
filtered.data <- autoFilter(bartlett2009, dn=c(5:7), filter=c('night', 'spline', 'max'),
                                         filter.options=NULL)
## same as before but returning unfilled data 
filtered.data <- autoFilter(bartlett2009, dn=c(5:7), 
	filter=c('night', 'spline', 'max'),
    filter.options=NULL, na.fill=FALSE)
## all filters in action (check the plot)
filtered.data <- autoFilter(bartlett2009, dn=c(5:7), 
	filter=c('night','blue', 'mad','spline', 'max'),
    filter.options=NULL)

## step 4: fit a function to the data 
data(bartlett2009.filtered)
## fit without uncertainty estimation Klosterman equation
fitted.kl <- KlostermanFit(bartlett2009.filtered, which='light')
## fit without uncertainty estimation Gu equation
fitted.gu <- GuFit(bartlett2009.filtered)
## check fitting
plot(bartlett2009.filtered)
lines(fitted.kl$fit$predicted, col='red')
lines(fitted.gu$fit$predicted, col='blue')

## step 5: define thresholds
data(bartlett2009.filtered)
## a fit: you could try instead of GuFit, BeckFit, KlostermanFit
fitted <- GuFit(bartlett2009.filtered)
## extract thresholds according to various methods
par(mfrow=c(5,1), mar=c(0,0,0,0), oma=c(5,4,4,4))
plot(bartlett2009.filtered, type='p')
lines(fitted$fit$predicted)
spl.trs <- PhenoExtract(fitted, method='spline', plot=TRUE, uncert=FALSE, yaxt='n')
axis(4) 
der.trs <- PhenoExtract(fitted, method='derivatives', plot=TRUE) 
klos.trs <- PhenoExtract(fitted, method='klosterman', plot=TRUE, yaxt='n')
axis(4) 
gu.trs <- PhenoExtract(fitted, method='gu', plot=TRUE) 

## uncertainty and plotting
data(bartlett2009.fitted)
klos.ths <- PhenoExtract(bartlett2009.fitted, method='gu', 
	plot=FALSE, uncert=TRUE, quantiles=c(.1, .9))
PhenoPlot(bartlett2009.fitted, klos.ths, 
	main='Klosterman fit and thresholds', ylab='GCC', xlab='DOY')

}
