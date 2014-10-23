\name{extractVIs}
\alias{extractVIs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts various vegetation indexes in a ROI
%%  ~~function to do ... ~~
}
\description{
This function allows to extract various vegetation indexes in a 
region of interest of a jpeg image. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extractVIs(img.path, roi.path, vi.path = NULL, roi.name = NULL, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img.path}{
  Path to images
%%     ~~Describe \code{img.path} here~~
}
  \item{roi.path}{
  Path to stored RData with ROI, see \code{DrawROI}
%%     ~~Describe \code{roi.path} here~~
}
  \item{vi.path}{
  Where to save the output. If NULL data are saved in the working directory. 
%%     ~~Describe \code{vi.path} here~~
}
  \item{roi.name}{
  The name of the ROI
%%     ~~Describe \code{roi.name} here~~
}
  \item{plot}{
  Should a diagnostic plot with red, green and blue indexes be returned?
%%     ~~Describe \code{plot} here~~
}
}
\details{
In the pixels contained in a ROI (which are loaded from the given folder), 
various vegetation indexes are calculated. Raw red, green and blue dn are averaged 
over the roi. The standard deviation is also calculated. For each pixel the 
brightness is computed as the sum of r + g + b. Then, for each pixel red, green 
and blue chromatic coordinates (aka relative indices) are calculated as 
r/brightness. After the calculation, all pixel data are averaged over the ROI. 
ExtractDateFilename is then called to get a time vector and a doy. The function 
applies recursively for all images in a folder and allows to get a multivariate 
time series of the various indexes. 
A plot is also generated and saved as .png if plot=TRUE 

Note: Brightness and relative color indices are calculated for each pixel and THEN 
averaged over the ROI. This is different from averaging the brightness and raw colors 
over the entire ROI and THEN calculate a ROI-based relative color index. This second 
approach is used within the function \code{autoFilter} in the case when ROI aggregated 
raw dn are provided and brightness is calculated from them. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with one data.frame for each ROI containing the multivariate time series 
of various vegetation indices. 
}
\author{
Edoardo Cremonese <e.cremonese@arpa.vda.it>
}