\name{get.options}
\alias{get.options}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns default options for function \code{autoFilter}. 
}
\description{
Returns default options for function \code{autoFilter}.
}
\usage{
get.options()
}
%- maybe also 'usage' for other objects documented here.
\details{
Returns default options for function \code{autoFilter}.
It can be assigned, changed and used within the function \code{autoFilter}. 
See examples for details

}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\examples{
data(bartlett2009)
## with raw.dn ==TRUE and column position for chromatic coordinates
filtered.data <- autoFilter(bartlett2009, dn=c(4:6), 
	filter=c('night', 'spline', 'max'),
    filter.options=NULL, raw.dn=TRUE)
my.options <- get.options()
## change time window for max filter
my.options$max.filter$w <- 5
filtered.data2 <- autoFilter(bartlett2009, dn=c(4:6), 
	filter=c('night', 'spline', 'max'),
    filter.options=my.options, raw.dn=TRUE)
plot(filtered.data$max.filtered)
lines(filtered.data2$max.filtered, col='red')
}