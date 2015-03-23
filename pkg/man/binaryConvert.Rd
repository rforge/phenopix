\name{binaryConvert}
\alias{binaryConvert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert all RGB images in a folder to binary black and white images
}
\description{
This function allows to convert all RGB images in a folder to binary black and white images   
}
\usage{
binaryConvert(ipath, opath)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ipath}{
  The absolute path to the folder where JPEG RGB images are stored. Make sure you include '/' at the end of the path. 
}
  \item{opath}{
  The absolute path to the folder where JPEG binary images will be stored. Make sure you include '/' at the end of the path. 
}

}
\details{
This function operates recursively within a folder containing ONLY JPEG images, reads them with the \code{readJPEG()} function, converts them to greyscale following the equation:   0.2126 * R + 0.7152 * G + 0.0722 * B and then round it to 0 and 1 (binary image). The data structure of the image is also changed. It was an array of RGB values in origin, adn becomes a simple matrix of 0 and 1 at the end of the process. Converted images are saved as JPEG again in the folder provided in opath. Binary images will be used to extract exposure values with \code{getCoords()} and \code{getExposure()} functions.  
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\seealso{
\code{\link{getCoords}}
\code{\link{getExposure}}
}


