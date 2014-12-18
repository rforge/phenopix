\name{spatialGreen}
\alias{spatialGreen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process pixel-based greenness indexes
}
\description{
This function allows to filter, fit a curve and extract thresholds in a 
pixel-based analysis exactly as \code{autoFilter} and \code{greenProcess} 
do in a ROI-based analysis, except that uncertainty cannot be estimated 
(since it would be too computationally intense)
}
\usage{
spatialGreen(spatial.list, fit, threshold, filters='default',
  parallel=TRUE, save=FALSE, path=NULL, assign=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spatial.list}{
  A list as in output from extractVIs with option \code{spatial} = TRUE. If you have extracted 
  more than one ROI, \code{spatial.list} must contain data from one single of such ROIs. Hence you have to run a spatialGreen
  analysis for each of your ROIs separately.  
%%     ~~Describe \code{img.path} here~~
}
  \item{fit}{
    A character vector of length 1. Available options are: \code{spline}, \code{beck}, \code{elmore},
  \code{klosterman}, \code{gu}. 
%%     ~~Describe \code{roi.path} here~~
}
  \item{threshold}{
  A character vector of length 1. Available options are: \code{spline}, \code{derivatives}, 
\code{klosterman}, \code{gu}.  

%%     ~~Describe \code{vi.path} here~~
}
  \item{filters}{
 Character names for filters. Chose one (or more) between 'night',
  'max', 'spline', 'blue' and 'mad'. The order provided in this argument
  determines the order of application of filters. See details of function \code{autoFilter} 
  for further information on filters. 
  The option 'default' means default values in \code{autoFilter}, 
  i.e. c('night', 'spline', 'max').  
%%     ~~Describe \code{roi.name} here~~
}
  \item{parallel}{
  To exploit parallel computation, defaults to TRUE. It calls functions in packages \code{foreach}, 
  \code{parallel} and \code{doParallel} and splits the process on (n.processors -1), so that one 
  processor remains available for your work. 
%%     ~~Describe \code{plot} here~~
}
  \item{save}{
  If TRUE it allows saving an .Rdata for each processed pixel in a folder specified in \code{path}. 
  Useful to track the process. Defaults to FALSE  
%%     ~~Describe \code{plot} here~~
}
  \item{path}{
  Path where to save your .Rdata for each pixel if save=TRUE. If you have processed multiple ROIs remember to
  provide  
%%     ~~Describe \code{plot} here~~
}
  \item{assign}{
  If TRUE (default) an object is assigned and returned. This may lead to memory saturation.
%%     ~~Describe \code{plot} here~~
}

}
\details{
This function allows to filter, fit a curve and extract thresholds in a 
pixel-based analysis exactly as \code{autoFilter} and \code{greenProcess} 
do in a ROI-based analysis, except that uncertainty cannot be estimated 
(since it would be too computationally intense). This function takes as first argument 
a list as in output from extractVIs (only with \code{spatial} = TRUE). For each pixel 
in the ROI the function filters out gcc data (according to options specified in 
\code{filters}, fit a curve (according to options specified in \code{fit}) and 
extracts thresholds (as defined in \code{threshold}). This function performs the same task 
that \code{autoFilter} and \code{greenProcess} 
do in a ROI-based analysis, except that uncertainty cannot be estimated 
(since it would be too computationally intense). 
For pixel-based analysis, it is recommended to use rather low resolution images. Consider 
that a full image of 640 x 428 pixels in which you extract a ROI that is about half the image 
leads to about 70k pixels. 70k pixels are in turn processed in about 72 hours on a rather fast 
computer with parallelization (on three cores). Parallel computation makes the effort lower. 
Another issue of this function is that it generates large objects which saturate the CPU memory. 
Therefore, it can be usefull to set \code{save} == TRUE, provide a \code{path} where to store each pixel's analysis
and not to assign the object (\code{assign} == FALSE). In this way, computation remains quite fast because 
memory and swap (unix systems) is saved. A function \code{extactParameters} is designed to extract fitting parameters
from \code{spatialGreen} output be it from an assigned R object or from the folder where temporary RData from each pixel 
are stored.   
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

