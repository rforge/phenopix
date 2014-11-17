\name{FitDoubleLogKlLight}
\alias{FitDoubleLogKlLight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fit a double logisitic function to a vector according 
to Klosterman et al. (2014) 
}
\description{
This function fits a double logistic curve to observed values using the
 function as described in Klosterman et al. (2014), equation 7. 
}
\usage{
FitDoubleLogKlLight(x, t = index(x), tout = t, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A vector or, better, an univariate ts or zoo object. 
}
  \item{t}{
  A vector of time (in numeric doys), if not provided index(x) is
  retrieved. 
}
  \item{tout}{
  For gapfilling pourposes, a vector of time steps at which the function
  can be predicted. 
}
  \item{\dots}{
  Further arguments currenlty unused. 
}
}
\details{
The function estimates parameters of the double logistic equation from
 Klosterman et al. 2014. The wrapper function \code{KlostermanFit} calls 
 this function and  additionally allows the calculation of uncertainty. 
 So better use  \code{KlostermanFit}. This function performs an optimization 
 similar in concept to the one performed in \code{FitDoubleLogKlHeavy} but 
 faster and in a less accurate manner. It it suggested to fit the light version 
 of the equation and if the fit is not good enought, check out \code{FitDoubleLogKlHeavy}.
}
\value{
    A list containing the following items. 
\item{predicted}{Predicted values from the equation}
\item{params}{Equation parameters}
\item{formula}{The equation}
}
\references{
Klosterman ST, Hufkens K, Gray JM, Melaas E, Sonnentag O, Lavine I, Mitchell L, 
Norman R, Friedl MA, Richardson A D (2014) Evaluating remote sensing of deciduous 
forest phenology at multiple spatial scales using PhenoCam imagery, Biogeosciences, 
11, 4305-4320, doi:10.5194/bg-11-4305-2014.
%% ~put references to the literature/web site here ~
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\examples{
data(bartlett2009.filtered)
fit <- FitDoubleLogKlLight(bartlett2009.filtered) ## about 10 seconds
## check fitting
plot(bartlett2009.filtered)
lines(fit$predicted, col='red')
## look at fitting parameters
fit$params
## look at fitting equation, where t is time
fit$formula
## recalculate fitted values from the formula
library(zoo)
t <- index(bartlett2009.filtered) ## the formula must get a time vector named t
predicted <- eval(fit$formula, envir=as.list(fit$params))
## and add them to the existing plot (hopefully it returns the same as red line!)
lines(t, predicted, col='blue', lwd=2)
}

