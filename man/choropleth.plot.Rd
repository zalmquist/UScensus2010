\name{choropleth.plot}
\alias{choropleth.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal Function
}
\description{
For internal use. 
}
\usage{
choropleth.plot(sp, dem = "P0010001", cuts = list("quantile", seq(0, 1, 0.25)), 
color = list(fun = "hsv", attr = list(h = c(0.4, 0.5, 0.6, 0.7), 
s = 0.6, v = 0.6, alpha = 1)), main = NULL, 
sub = "Quantiles (equal frequency)", 
legend = list(pos = "bottomleft", title = "Population Count"),type=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{
%%     ~~Describe \code{sp} here~~
}
  \item{dem}{
%%     ~~Describe \code{dem} here~~
}
  \item{cuts}{
%%     ~~Describe \code{cuts} here~~
}
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{sub}{
%%     ~~Describe \code{sub} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{type}{
%%     ~~Describe \code{legend} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\references{
Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}.
}
\author{
Zack W Almquist
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
