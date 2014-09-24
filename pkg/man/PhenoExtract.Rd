\name{PhenoExtract}
\alias{PhenoExtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function that extracts phenological thresholds
}
\description{
This function extracts phenological thresholds according to different methods. 
Methods include 'spline', 'derivatives', 'klosterman', 'gu'. See details for the 
computation of each method.  
}
\usage{
PhenoExtract(data, method = "spline", uncert = FALSE, breaks = 3, envelope = "quantiles", 
  quantiles = c(0.1, 0.9), plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  A list structured as in output from the fitting procedures, such as GuFit, 
  KlostermanFit, ElmoreFit, BeckFit. 
}
  \item{method}{
  One between 'spline', 'derivatives', 'klosterman', 'gu'.
}
  \item{uncert}{
  Should uncertainty on thresholds be computed? It requires that uncertainty 
  be computed in the fitting function. I.e. The function requires the element 
  'uncertainty' in data beeing non NULL. If is.null(uncertainty) in the data or 
  this item is set to FALSE, uncertainty won't be computed.  
}
  \item{breaks}{
Currently unused%%     ~~Describe \code{breaks} here~~
}
  \item{envelope}{
  One between 'quantiles' and 'min-max'. If 'quantiles', the uncertainty envelope 
  will be computed as quantiles. Quantiles reported in \code{quantiles} will be 
  computed togheter with the median. If 'min-max' is choosen, min, max and mean of 
  the uncertainty envelope will be returned. 
}
  \item{quantiles}{
  Quantiles to be calculated if envelope='quantiles'. The notation is the same as 
  to specify quantiles in the \code{quantile} function, i.e. to get 10th and 90th 
  percentile, use c(0.1, 0.9).  
}
  \item{plot}{
  Should a diagnostic plot be returned with annotated thresholds? It calls the 
  function \code{PhenoPlot}
}
  \item{\dots}{
  For the plotting function, a number of parameters from generic \code{plot} 
  can be specified. See examples. 
}
}
\details{
This is a wrapper function that calls 'PhenoTrs' for method='spline', 'PhenoDeriv' 
for method='deriatives', (from package greenbrown) or 'PhenoGu' for method='Gu',
 and 'PhenoKl' for method='klosterman' from this package. Please see help of the 
 single functions for details on the calculation of thresholds.  
}
\value{
If uncertainty=FALSE a vector of phenology metrics, otherwise a dataframe. 
}
\references{

Klosterman ST, Hufkens K, Gray JM, Melaas E, Sonnentag O, Lavine I, Mitchell L, 
Norman R, Friedl MA, Richardson A D (2014) Evaluating remote sensing of deciduous 
forest phenology at multiple spatial scales using PhenoCam imagery, Biogeosciences, 
11, 4305-4320, doi:10.5194/bg-11-4305-2014.

Gu L, Post WM, Baldocchi D, Black TA, Suyker AE, Verma SB, Vesala T, Wofsy SC. (2009) 
Characterizing the Seasonal Dynamics of Plant Community Photosynthesis Across a Range 
of Vegetation Types. In: Phenology of Ecosystem Processes (Ed: Noormets A, 
Springer New York), pp 35-58.

Zhang X, Friedl MA, Schaaf CB, Strahler AH, Hodges JCF, Gao F, Reed BC, Huete A (2003) 
Monitoring vegetation phenology using MODIS, Remote Sens. Environ., 84, 471-475.
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}

\seealso{
\code{\link{PhenoGu}},
\code{\link{PhenoKl}},
\code{\link{PhenoDeriv}},
\code{\link{PhenoTrs}},
\code{\link{PhenoPlot}},
\code{\link{PhenoGu}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bartlett2009.filtered)
## a fit: you could try instead of GuFit, BeckFit, KlostermanFit
fitted <- GuFit(bartlett2009.filtered)
## extract thresholds according to various methods
par(mfrow=c(5,1), mar=c(0,0,0,0), oma=c(5,4,4,4))
plot(bartlett2009.filtered, type='p')
lines(fitted$fit$predicted)
spl.trs <- PhenoExtract(fitted, method='spline', 
  plot=TRUE, uncert=FALSE, yaxt='n')
axis(4) 
der.trs <- PhenoExtract(fitted, method='derivatives', plot=TRUE) 
klos.trs <- PhenoExtract(fitted, method='klosterman', plot=TRUE, yaxt='n')
axis(4) 
gu.trs <- PhenoExtract(fitted, method='gu', plot=TRUE) 
}

