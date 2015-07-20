\name{city}
\alias{city}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects one or more CDP(s) from a given state
}
\description{
\code{city} allows the user to pull out the polygon and metadata of one or more CDPs for any given state by name.
}
\usage{
city(name, state, statefips = FALSE, 

sp.object = NULL, proj = NULL )
}
\arguments{
  \item{name}{
  a character string, takes the value of a string or string vector and has to be the exact name or names of CDP(s). (If you are unsure of the exact name a quick way to find it is to load the \code{library(UScensus2010cdp)} and pull out the list of names for the state you are interested in (see example).Note: if statefips=TRUE then this must be a CDP fips code.
}
  \item{state}{
a character string, can either be the full name (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you have to change \code{statefips} to \code{TRUE}.
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
%%\details{}
\value{
An object of class \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}.
}
\references{
Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. \cr \cr

Census 2010 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2011. \cr
\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}
}
\author{
Zack W. Almquist \email{almquist@uci.edu}
}
%%\note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{}
\examples{
\dontrun{
##Load oregon.cdp10 data 
data(oregon.cdp10)

##look at the list of oregon CDP names in alphabetic order
oregon.cdp10$name[order(oregon.cdp10$name)]

##grab the CDP of Portland, OR
portland<-city(name="portland",state="or")


##plot the polygon of Portland, OR
plot(portland)
title("Portland, OR")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{shapefiles}
\keyword{CDP}
\keyword{uscensus}
\keyword{demography}
\keyword{census}% __ONLY ONE__ keyword per line
