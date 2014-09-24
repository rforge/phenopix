\name{FitDoubleLogBeck}
\alias{FitDoubleLogBeck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A fit
%%  ~~function to do ... ~~
}
\description{
See FitDoubleLogBeck from package greenbrown
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FitDoubleLogBeck(x, t = index(x), tout = t, weighting = TRUE, 
    return.par = FALSE, plot = FALSE, ...)
}
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
  \item{weighting}{
  Should fit be weighted? 
}
  \item{return.par}{
  Currently unused. 
}
  \item{plot}{
  Currently unused. 
}
  \item{\dots}{
  Further arguments currenlty unused. 
}
}