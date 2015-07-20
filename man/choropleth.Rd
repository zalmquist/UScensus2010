\name{choropleth}
\alias{choropleth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Choropleth Mapping
}
\description{
A function geared to making choropleth maps easier to construct for the US Census Data.
}
\usage{
choropleth(sp, dem = "P0010001", 
cuts = list("quantile", seq(0, 1, 0.25)), 
color = list(fun = "hsv", attr = list(h = c(0.4, 0.5, 0.6, 0.7),
 s = 0.6, v = 0.6, alpha = 1)), main = NULL, 
 sub = "Quantiles (equal frequency)", 
 border = "transparent", 
 legend = list(pos = "bottomleft", 
 title = "Population Count"), type = "plot", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{ \code{SpatialPolygonsDataFrame}, must be a \code{SpatialPolygonsDataFrame} object. }
  \item{dem}{ a character string, this must be the name of one of the \code{data.frame} objects contained within the \code{SpatialPolygonsDataFrame} (e.g. "P0010001").}
  \item{cuts}{ a list containing "quantile" and \code{seq} object from 0 to 1.}
  \item{color}{a list containing a function and list of arguments for the function to produce the requested color scheme. }
  \item{main}{a character string, this will be the title of the plot.
}
  \item{sub}{
a character string, this will be the subtitle on the plot.
}
  \item{border}{a character string, this selects the border color of the polygons.}
  \item{legend}{a list containing first where to place the legend and second a title for the legend.}
  \item{type}{a character string, can be either "plot" or "spplot".}
  \item{\dots}{
Only arguments available in \code{\link{plot}}. 
}
}
\details{
\code{choropleth} is simply a convenient front end for \code{\link{plot}} and \code{\link{spplot}} specifically for use in making quick choropleth maps of US Census data.
}
\value{
a plot or lattice object.
 }
\references{
 Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}.
 
 }

\author{
Zack W Almquist
}
%\note{

%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{spplot}}, \code{\link{plot}}
}
\examples{
\dontrun{
data(oregon.county10)

###Using plot
choropleth(oregon.county10,"P0010001",
color = list(fun = "rainbow", 
attr = list(4)),main="2010 Counties \n Oregon",type="plot",border="transparent")



###Using spplot
choropleth(oregon.county10,"P0010001",
main="2010 Counties \n Oregon",border="transparent",type="spplot")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ choropleth}

