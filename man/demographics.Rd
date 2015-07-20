\name{demographics}
\alias{demographics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects a specified demographic meta-data from the sp objects.
}
\description{
\code{demographics} allows the user to pull out one or more demographic variables at several different geographic levels.
}
\usage{
demographics(dem = "P0010001", state, statefips=FALSE, 
level = c("tract", 
"blk", "blkgrp", "cdp", "msa", "county"), msaname=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dem}{Character string or vector. Must be the actual name of the demographic variables attached to \code{UScensus2010} objects. Default \code{dem} = " P0010001"
}
  \item{state}{
a character string,  can either be the full name of a state (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you have to change statefips to \code{TRUE}. This variable is insensitive to case.
}
  \item{statefips}{
logical, by default \code{statefips=FALSE}, set to \code{TRUE} if using the state FIPS codes.
}
  \item{level}{
a character string, takes levels \code{tract, blk, blkgrp, cdp, msa} or \code{county}
}
  \item{msaname}{
logical (optional), if \code{level="msa"}, allows the use of the verbose MSA placename (capitalized).
}
}
%%\details{}
\value{
An object of class \code{\link[base:matrix]{matrix}}.
}
\references{
Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. \cr \cr

Census 2010 Summary File 1 [name of state1 or United States]/prepared by the U.S. Census 
Bureau, 20010. \cr
\url{http://www.census.gov/prod/cen2010/doc/sf1.pdf}
}
\author{Zack W. Almquist \email{almquist@uci.edu}}
%\note{}
\section{Warning }{You must have the packages \code{UScensus2010tract}, \code{UScensus2010blkgrp} and \code{UScensus2010blk} installed to use levels "blkgrp" and "blk" respectively.
} 

\seealso{
\code{\link{county}}, \code{\link{MSA}}, \code{\link{city}}
}
\examples{
\dontrun{
#County Example
demographics(state="or",level="county")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{demographics}
\keyword{uscensus}
\keyword{census}% __ONLY ONE__ keyword per line
