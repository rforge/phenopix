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
various vegetation indexes are calculated. Red, green and blue dn are averaged 
over the roi. The standard deviation is also calculated. From mean rgb digital 
numbers the ROI brightness is computed (r + g + b). Color chromatic coordinates 
are then calculated dividing each color number by brightness. 
ExtractDateFilename is then called to get a time vector and a doy. The function 
applies recursively for all images in a folder and allows to get a multivariate 
time series of the various indexes. 
A plot is also generated and saved as .png if plot=TRUE 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with one data.frame for each ROI containing the multivariate time series 
of various vegetation indices. 
}
\author{
Edoardo Cremonese <e.cremonese@arpa.vda.it>
}