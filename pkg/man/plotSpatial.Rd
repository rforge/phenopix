\name{plotSpatial}
\alias{plotSpatial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot pixel-based analysis results from spatialGreen and extractParameters functions 
}
\description{
This function allows to plot results from pixel-based analysis of a seasonal ensemble of images in one or multiple rois.   
}
\usage{
plotSpatial(data, param, roi.data.path, roi.name=NULL, image.path, probs=c(0.01, 0.99), plot.density=TRUE, digits=0) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  	Either a data.frame or a list of dataframes
}
  \item{param}{
  	Character string of length 1 with name of the parameter you want to plot. It must match one of names(data)
}
  \item{roi.data.path}{
  	The complete path where to find roi data (e.g. '/home/me/folderROI/roi.data.RData')
}
  \item{roi.name}{
  	Supply the name (character string) of the roi you want to plot, as named in your roi.data.RData. If NULL 
  	and your roi.data.RData contains only one ROI, than this one is choosen. If you have more than one roi but 
  	you want to plot only one, then specify here the name of the roi. 
}
  \item{image.path}{
  	A complete path of an image you want to use as background for plotting your data. E.g. 
  	'/home/me/images/my_site201406061200.jpg'
}

  \item{probs}{
  	Two numbers in the range [0,1] passed to \code{quantile()} function to remove tails of the distribution for the plot.  
}
  \item{plot.density}{
  	If TRUE a density distribution of values is plotted in the upper plotting region
}
  \item{digits}{
  Integer specifying decimal number for key labels
} 

}
\details{
	This function allows to plot on a reference image results from the pixel based extraction of thresholds and other relevant parameters from pixel based fitting. Multiple ROIs are allowed and handled as well. On  top of the plot the density distribution of value for each pixel is shown, if desired. Values are plotted in a black to green scale, with a legend. Data in entrance to this function can be stored in a data.frame (single ROI data) as in output from \code{extractParameters}. If you have analyised more than one ROI and you want them to be shown together, put individual data.frames in output from \code{extractParameters} in a list and pass the list as data to \code{plotSpatial}.   
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}



