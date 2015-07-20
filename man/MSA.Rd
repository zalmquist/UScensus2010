\name{MSA}
\alias{MSA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects one MSA from a given state.
}
\description{
\code{MSA} allows the user to pull out the polygon and metadata of one MSA from any given state for any of three levels: tract, blockgroup, or block.
}
\usage{
MSA(msafips = NULL, msaname = NULL, state=NULL , 
statefips=FALSE, level = c("tract", "blk", "blkgrp"), proj = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{msafips}{
  a character string, takes a four digit MSA FIPS code (e.g. "0040" of Texas)
  }
  \item{msaname}{
a character string, this can either be in conjunction with the variable \code{state} or not. Case 1: Full MSA name (\code{state} should be left \code{NULL} in this case) (e.g. "Abilene, TX MSA"); this must be exact. Case 2: takes one of the city names of the MSA and the one of the states which contain the MSA (e.g. \code{msaname}="Albany" and \code{state}= "NY").
}
\item{state}{
a character string, this takes in a state abbreviation in capitals in conjunction with \code{msaname}, see above for more details.
}
\item{statefips}{ logical, by default \code{statefips=FALSE}, change to \code{TRUE} when providing \code{state} with a FIPS code.
}
  \item{level}{
a character string, takes in one of three values: "tract", "blk", or "blkgrp". This defines the geographic level of data for the MSA.
}
\item{proj}{ CRS-class, takes a CRS object (e.g. CRS("+proj=utm +zone=10 +datum=NAD83") ); This is simply a wrapper for the \code{spTransform} function in \code{rgdal} . WARNING requires \code{\link[rgdal:spTransform]{rgdal}} package.
}
}
%%\details{}
\value{
An object of class \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}.
}
\references{
Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. \cr \cr

Census 2000 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2001. \cr
\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}
}
\author{Zack W. Almquist \email{almquist@uci.edu}
}
%%\note{}

\section{Warning }{You must have the packages \code{UScensus2000blkgrp} and \code{UScensus2000blk} installed to use levels "blkgrp" and "blk" respectively.} 


\seealso{
\code{\link{county}}
}
\examples{
\dontrun{
## Load the data files for MSA names and MSA FIPS codes
data(MSAnames)
data(MSAfips)

## Save the FIPS code for Abilene, TX MSA
ab.fips<-MSAfips$msa.cmsa.fips[1]

###Use the MSA FIPS code
Abilene<-MSA(msafips=ab.fips,level="tract")

###USe the MSA full name
Abilene<-MSA(msaname="Abilene, TX MSA",level="tract")

##Use the msaname alternative
portland<-MSA(msaname="Portland",state="OR",level="tract")


##Plot Portland
plot(portland)
title("Portland MSA, OR 2000")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{shapefiles}
\keyword{demography}
\keyword{msa}% __ONLY ONE__ keyword per line
