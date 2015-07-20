\name{nameTofips}
\alias{nameTofips}
\title{
County or MSA name to FIPS code(s).
}
\description{
Takes the name of a county or msa and returns the associated fip(s) codes.
}
\usage{
nameTofips(name, state, type = c("county", "msa"), statefips = FALSE)
}
\arguments{
  \item{name}{a character string, should be either a name of a county or msa.}
  \item{state}{a character string, can either be the full name (e.g. "oregon"), the abbreviation (e.g. "or"), or the FIPS code (e.g. "41")-- note that if you are using the FIPS code you have to change \code{statefips} to \code{TRUE}.}
  \item{type}{a character string, should be either "county" or "msa" 
}
  \item{statefips}{logical, by default \code{statefips=FALSE}, change to \code{TRUE} when providing \code{state} with a FIPS code.
}
}
\value{
Returns a character string.
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

\examples{
\dontrun{
## SF MSA FIPS Code
nameTofips("san francisco","ca","msa")

## Orange County FIPS
nameTofips("orange","ca","county")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
