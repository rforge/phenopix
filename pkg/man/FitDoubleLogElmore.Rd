\name{FitDoubleLogElmore}
\alias{FitDoubleLogElmore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A fit
%%  ~~function to do ... ~~
}
\description{
See FitDoubleLogElmore from package greenbrown
}
\usage{
FitDoubleLogElmore(x, t = index(x), tout = t, 
    return.par = FALSE, plot = FALSE, hessian=FALSE, ...)
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
  \item{return.par}{
  Currenlty unused. 
}
  \item{plot}{
  Currenlty unused. 
}  
\item{hessian}{
  Currently unimplemented. 
}
  \item{\dots}{
  Further arguments currenlty unused. 
}
}