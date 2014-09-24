\name{extractDateFilename}
\alias{extractDateFilename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estract dates from filenames 
%%  ~~function to do ... ~~
}
\description{
This function extracts dates from filenames. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extractDateFilename(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
  The filename where to retrieve time stamp 
%%     ~~Describe \code{path_img} here~~
}
}
\details{
This function allows the extraction of the date (hour, doy, dayfract) 
from the filename. The file name can contain character but the date in 
the name MUST follow the following order: year, month, day, hour, minutes. 
Eg of valid filename: 
sitename_2012_03_03_15-30.jpg
201203031530.jpg
}
\value{
A POSIX string containing date,Hour,DOY,DOY.dayfract of the entire images time series
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Edoardo Cremonese <e.cremonese@arpa.vda.it>
}

