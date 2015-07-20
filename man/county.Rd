\name{county}
\alias{county}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects one or more counties in a given state
}
\description{
\code{county} allows the user to pull out the polygon and metadata of one or more county(s) from a given state.
}
\usage{
county(fips = NULL, name = NULL, state, 

level = c("tract", "blk", "blkgrp"), statefips = FALSE, sp.object=NULL, proj=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fips}{a character string, takes a string of three characters (i.e. a county FIPS code (e.g. "001").
}
  \item{name}{
a character string, this must be the name of an actual county in the state (e.g. "Baker" county Oregon). This variable is insensitive to case.
}
  \item{state}{a character string, can either be the full name (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you  have to change \code{statefips} to \code{TRUE}. This variable is insensitive to case.
}
  \item{level}{
a character string, takes in one of three values: "tract", "blk",  or "blkgrp". This defines the geographic level of data for the county.
}
  \item{statefips}{
logical, by default \code{statefips=FALSE}, change to \code{TRUE} when providing \code{state} with a FIPS code.
}
\item{sp.object}{\code{
SpatialPolygonsDataFrame}, default \code{NULL}, allows the user to provide an sp object in which to perform this operation; primarily for use with \code{demographics.add}.
}
\item{proj}{ CRS-class, takes a CRS object (e.g. CRS("+proj=utm +zone=10 +datum=NAD83") ); This is simply a wrapper for the \code{spTransform} function in \code{rgdal} . WARNING requires \code{\link[rgdal:spTransform]{rgdal}} package.
}
}
%\details{}
\value{
An object of class \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}.
}
\references{
Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. \cr \cr

Census 2010 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2011. \cr
\url{http://www.census.gov/prod/cen2010/doc/sf1.pdf}
}
\author{Zack W. Almquist \email{almquist@uci.edu}
}
%%\note{ }

\section{Warning }{You must have the packages  \code{UScensus2010tract}, \code{UScensus2010blkgrp} and \code{UScensus2010blk} installed to use levels "tract", "blkgrp" and "blk" respectively.} 


%%\seealso{}
\examples{
\dontrun{
#### look at the counties of Oregon
data(countyfips)
countyfips[countyfips$statename=="oregon",]

### The county fips code is the last three characters
county.f<-"001"
county.n<-c("deschutes","crook county")

## Pull out these counties
c1<-county(fips=county.f,state="or",level="tract")
c2<-county(name=county.n,state="or",level="tract")

##Plot counties
oregon.counties<-countyfips[countyfips$statename=="oregon",]
col<-cbind(c("red","blue"),c("013","017"))
plot(c2,col=col[match(c2$county,col[,2]),1],border="gray")
title("Deschutes and Crook counties, OR 2000")
coord<-coordinates(c2)
text(coord[c(1,4),],oregon.counties$countyname[oregon.counties$countyname\%in\%county.n],cex=2)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{shapefiles}
\keyword{demography}
\keyword{county}% __ONLY ONE__ keyword per line
