\name{PhenoPlot}
\alias{PhenoPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A plotting function for phenological thresholds
}
\description{
This function uses data from a fitted phenological model, the extracted 
metrics and plots them in an annotated graph. 
}
\usage{
PhenoPlot(data, metrics, add = FALSE, show.uncert = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Fitted data from a fitting function such as GuFit, SplineFit, KlostermanFit, 
ElmoreFit, BeckFit
}
  \item{metrics}{
  A named vector or a dataframe, depending on the presence or absence of 
  uncertainty estimation 
}
  \item{add}{
  If TRUE the plot is superimposed to an existing one
%%     ~~Describe \code{add} here~~
}
  \item{show.uncert}{
Should uncertainty be shown as error bars around the extracted thresholds? 
It requires that data in imput incorporate the uncertainty estimation.   
%%     ~~Describe \code{show.uncert} here~~
}
  \item{\dots}{
Several argument as in \code{plot} can be specified here, see example.   
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This function allows to plot fitting and thresholding on a season of gcc 
data. Uncertainty can be also shown
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}
\seealso{
\code{\link{PhenoExtract}}
}

\examples{
data(bartlett2009.filtered)
## a fit: you could try instead of GuFit, BeckFit, KlostermanFit
fitted <- KlostermanFit(bartlett2009.filtered, which='light')
klos.ths <- PhenoExtract(fitted, method='klosterman')
PhenoPlot(fitted, klos.ths)
## may be added to an existing plot
plot(bartlett2009.filtered)
PhenoPlot(fitted, klos.ths, add=TRUE)
## PhenoPlot accepts a number of options from function plot
PhenoPlot(fitted, klos.ths, main='Klosterman fit and thresholds', 
	ylab='GCC', xlab='DOY')

}
