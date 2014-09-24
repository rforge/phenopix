\name{PhenoBP}
\alias{PhenoBP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to extract breakpoints on a time series
}
\description{
A function to extract breakpoints and confidence intervals on a time series
}
\usage{
PhenoBP(x, breaks=3, confidence=0.95, plot=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An univariate ts or zoo object 
}
  \item{breaks}{
Maximum number of breaks to be detected
}
  \item{confidence}{
The confidence level for the uncertainty computation. Defaults to 0.95, i.e. 
95\% confidence interval (two-tails). 
}
  \item{plot}{
Should a diagnostic plot be returned?
}
  \item{\dots}{
  Arguments passed to \code{plot}. 
}
}
\details{
Threshold extraction is performed according to breakpoint analysis. 
The function used is \code{breakpoints} from package \code{strucchange}. 
The function also computes a confidence interval by a call to \code{confint()}. 
Uncertainty analysis is therefore different from the approach used for other 
thresholds (see e.g. \code{PhenoGu}). Unlike the other thresholding approaches, 
PhenoBP is born to work with raw data. It therefore does not require fitting 
an equation. However, the function can also be used with fitted data. See example.   
}
\value{
A named vector with extracted breakpoints
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}

\seealso{
\code{\link{PhenoExtract}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(bartlett2009.fitted)
data(bartlett2009.filtered)
par(mfrow=c(3,1), mar=c(0,4,0,2), oma=c(5,0,4,0))
plot(bartlett2009.filtered, type='p')
lines(bartlett2009.fitted$fit$predicted)
PhenoBP(bartlett2009.filtered, ylab='Break points on raw data')
PhenoBP(bartlett2009.fitted$fit$predicted, ylab='Break points on fitting curve')
}
