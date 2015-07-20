\name{poly.clipper}
\alias{poly.clipper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects the block, block group, or tracts which are contained within a given CDP.
}
\description{
\code{poly.clipper} allows the user to pull out all the blocks, block groups, or tracts that fall within the boundary of a given CDP and where the above grouping falls outside the boundaries  \code{poly.clipper} will output the intersection of the two polygons and estimate the demographic variables based on the area of the intersection.
}
\usage{
poly.clipper(name, state, statefips = FALSE, 
level = c("tract", "blk", "blkgrp"), sp.object = NULL, proj = NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{a character string, this string must be the name of CDP for a given state (if statefips=TRUE then this must be a CDP fips code).
}
  \item{state}{
a character string, can either be the full name of a state (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you have to change \code{statefips} to \code{TRUE}. This variable is insensitive to case.
}
  \item{statefips}{
 logical, by default \code{statefips=FALSE}, change to \code{TRUE} when providing \code{state} with a FIPS code.
}
  \item{level}{
character string, takes in one of three values: "tract", "blk", or "blkgrp". This defines the geographic level of data for the county.
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

Census 2000 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 2001. \cr
\url{http://www.census.gov/prod/cen2000/doc/sf1.pdf}
}
\author{Zack W. Almquist \email{almquist@uci.edu}
}
%%\note{ }

\section{Warning }{You must have the packages   \code{UScensus2010cdp}, \code{UScensus2010tract}, \code{UScensus2010blkgrp} and \code{UScensus2010blk} installed to use levels "blkgrp" and "blk" respectively.} 


\seealso{
\code{\link{city}} \cr
% \code{\link[UScensus2000cdp]{SpatialPolygonsDataFrame}}.
}
\examples{
\dontrun{
##Perform the clipping
portland<-poly.clipper(name="Portland",state="OR",level="tract")

######Plot Portland as a Choropleth map

############################################
## Helper function for handling coloring of the map
############################################
color.map<- function(x,dem,y=NULL){
	l.poly<-length(x@polygons)
	dem.num<- cut(dem ,breaks=unique(ceiling(quantile(dem))),dig.lab = 6)
	dem.num[which(is.na(dem.num)==TRUE)]<-levels(dem.num)[1]
	l.uc<-length(table(dem.num))
if(is.null(y)){
	##commented out, but creates different color schemes 
	## using runif, may take a couple times to get a good color scheme.
	##col.heat<-rgb( runif(l.uc,0,1), runif(l.uc,0,1) , runif(l.uc,0,1) )
	col.heat<-heat.colors(16)[c(14,8,4,1)] ##fixed set of four colors
}else{
	col.heat<-y
	}
dem.col<-cbind(col.heat,names(table(dem.num)))
colors.dem<-vector(length=l.poly)
for(i in 1:l.uc){
	colors.dem[which(dem.num==dem.col[i,2])]<-dem.col[i,1]
	}
out<-list(colors=colors.dem,dem.cut=dem.col[,2],table.colors=dem.col[,1])
return(out)
}
############################################
## Helper function for handling coloring of the map
############################################

colors.use<-color.map(portland,portland$pop2000)
plot(portland,col=colors.use$colors)
#text(coordinates(alabama.blk),alabama.blk@data$name,cex=.3)
title(main="Census Tracts of\n Portland Oregon, 2000", 
sub="Quantiles (equal frequency)")
legend("bottomright",legend=colors.use$dem.cut,
fill=colors.use$table.colors,bty="o",title="Population Count",bg="white")

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{CDP}
\keyword{demography}% __ONLY ONE__ keyword per line
\keyword{chapefile}
\keyword{polygon}
\keyword{clipping}

