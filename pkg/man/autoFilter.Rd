\name{autoFilter}
\alias{autoFilter}
\title{
A multiple-approach filtering procedure
}
\description{
5 different filters can be applied to raw green coordinate
values. Filters can be applied alone or in sequence, in this case order
matters. Available filters are 'night', 'max', 'spline', 'blue' and
'mad'. See details for further explaination. 
}
\usage{
autoFilter(data, dn, raw.dn = TRUE, brt = NULL, na.fill = TRUE, 
  filter = c("night", "spline", "max"), filter.options = NULL, plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  A data.frame containing a POSIX vector 
}
  \item{dn}{
The column positions or colnames for red, green and blue digital numbers, in this order.
}
  \item{raw.dn}{
 If TRUE gcc is calculated on digital numbers, which position or colnames must be
  provided in \code{dn}. If false it will be calculated as the sum of green red and blue 
  digital numbers. 
}
  \item{brt}{
  The column position for brightness, needed only if gcc is not yet calculated.
}
  \item{na.fill}{
  If FALSE, discarded data are filled with NA. If TRUE a call to \code{na.approx} from 
  package zoo linearly interpolates between existing records up to a gap of 10 missing values. 
}
  \item{filter}{
  Character names for filters. Chose one (or more) between 'night',
  'max', 'spline', 'blue' and 'mad'. The order provided in this argument
  determines the order of application of filters. See details for further information on filters.
}
  \item{filter.options}{
  If NULL filter.options defaults to set values, otherwise they must be
  specified in a named list. Filter options are designed to work with a wide range of data, 
  the user must change them with caution.  
}
  \item{plot}{
    If TRUE a dignostic plot of the different filtering effects is
  returned. 
}
  \item{\dots}{
  Further options, currenlty not used 
%%     ~~Describe \code{\dots} here~~
}
}

\details{
  This function takes a data.frame with raw digital numbers of red green and blue found in \code{dn}, 
  converts them in the respective coordinates (example for red: rcc=r/(r+g+b)). If \code{raw.dn}=FALSE, 
  the function takes directly color's chromatic coordinates provided in \code{dn}. 
  In the dataframe a POSIX vector must be provided, which will subsequently converted in numeric 
  day of year (doy). Afterwards, the filtering procedure starts.  
  Filters are applied in the order provided in \code{filter}.
  Night filter removes records under a certain gcc
  value (as specified in filter.options). The default is 0.2.
  Blue filter is intended to remove bad images and is very aggressive.
  It is suggested only for very low quality images. The daily mean and standard deviation 
  on bcc is computed and a sd threshold is computed as the quantile of standard 
  deviations with prob = 0.05. An envelope is then computed as daily mean bcc +/- 
  the calculated threshold. Raw data outside this envelope are discarded. 
  The mad filter is applied according to Papale et al 2006 (biogeosciences) created 
  to remove spikes on FLUXNET data. 
  The max filter is based on Sonnentag et al (2012) and computes the 90\% of the 
  curve based on a three days moving window. 
  The spline filter is based on Migliavacca et al (2011).  
}
\value{

  A multivariate zoo object with raw data and a column with gcc values after
  filtering. Colnames for filtered data have the same name as the
  applied filter. 
}
\references{
Sonnentag et al. 

Migliavacca et al. 

Papale et al.
}
\author{
  Gianluca Filippa <gian.filippa@gmail.com>
}

\examples{
data(bartlett2009)
## with raw.dn ==TRUE and column position for chromatic coordinates
filtered.data <- autoFilter(bartlett2009, dn=c(4:6), filter=c('night', 'spline', 'max'),
                                         filter.options=NULL)
## same as before but returning unfilled data 
filtered.data <- autoFilter(bartlett2009, dn=c(4:6), filter=c('night', 'spline', 'max'),
                                         filter.options=NULL, na.fill=FALSE)
## all filters in action (check the plot)
filtered.data <- autoFilter(bartlett2009, dn=c(4:6), filter=c('night','blue', 'mad','spline', 'max'),
                                         filter.options=NULL)
}

