\name{MSAnames}
\alias{MSAnames}
\docType{data}
\title{
MSA Names and FIPS codes
}
\description{
MSA names and FIPS codes for use in  \code{\link{MSA}}
}
\usage{data(MSAnames)}
\format{
  A data frame with 356 observations on the following 3 variables.
  \describe{
    \item{\code{msa.cmsa.fips}}{a character vector}
    \item{\code{pmsa.fips}}{a character vector}
    \item{\code{name}}{a character vector}
  }
}
\details{
Metropolitan areas and components, 1999. Built from \url{http://www.census.gov/population/estimates/metro-city/99mfips.txt}. This is primarily for use in \code{\link{MSA}}.
}
\source{
\url{http://www.census.gov/population/estimates/metro-city/99mfips.txt}
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(MSAnames)

}
\keyword{datasets}
