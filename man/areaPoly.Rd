\name{areaPoly}
\alias{areaPoly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Area of the polygons in SpatialPolygonsDataFrame
}
\description{
Calculates the area of each polygon in \code{SpatialPolygonsDataFrame}.
}
\usage{
areaPoly(sp.object=NULL) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp.object}{
\code{SpatialPolygonsDataFrame}, must be a \code{SpatialPolygonsDataFrame} object. 
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a numeric vector.
}
\references{

Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. \cr \cr

Also see: \link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}
}
\author{
Zack W. Almquist
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
\dontrun{
data(oregon.county10)



## Build density using areaPoly()
den00<-oregon.county10$P0010001/areaPoly(oregon.county10)
oregon.county10$den00<-den00

choropleth(oregon.county10,"den00",color = list(fun = "rainbow", 
attr = list(4)),main="2010 US Counties \n 
Oregon",type="plot",border="transparent")
}




}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
