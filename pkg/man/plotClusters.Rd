\name{plotClusters}
\alias{plotClusters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns an image with cluster map in the ROI
}
\description{
This function plots results of the cluster analysis on a reference image.     
}
\usage{
plotClusters(input, roi.data.path, image.path, upadj = NULL, 
  rightadj = NULL, legend=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
An object in output from the function \code{greenClusters} 
}
  \item{roi.data.path}{
The complete path where to find roi data (e.g. '/home/me/folderROI/roi.data.RData')
}
  \item{image.path}{
  	A complete path of an image you want to use as background for plotting your data. E.g. 
  	'/home/me/images/my_site201406061200.jpg'
}
  \item{upadj}{
  Integer specifying how to handle the upper margin of the density plot, relevant only if plot.density == TRUE. 
  It can be manually changed according to the shape of your plotting window.  
} 
  \item{rightadj}{
  Integer specifying how to handle the right margin of the density plot, relevant only if plot.density == TRUE.
  It can be manually changed according to the shape of your plotting window.  
} 
  \item{legend}{
If TRUE a legend is added.  
} 
}
\details{
This function allows to split a large ROI into smaller ones. This is needed to make computationally 
feasible the pixel-based analysis with large ROIs and/or high resolution images. As a general indication, 
approx 10000 pixels per sub-ROI is the upper limit for a computationally feasible spatial analysis, 
so set nsplit argument accordingly. The number of pixels in a ROI essentially controls the dimension of the VI.data
object you generate with \code{extractVIs} function. A large VI.data object (typically larger than 200 Mb) will fast 
saturate your RAM and slow down the spatial analysis.
}
\value{
A names list of length = nsplit and where each element has the same structure as a roi.data extracted by \code{DrawROI}. 
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}



