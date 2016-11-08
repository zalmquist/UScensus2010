\name{CensusAPI2010Spatial}
\alias{CensusAPI2010Spatial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
US Census API function
}
\description{
Allows the user to access all Summary File 1 (SF1; 100-percent data; \url{http://www.census.gov/prod/cen2010/doc/sf1.pdf}) and American Community Survey (ACS; economic data). Requires the user to sign up for US Census API key, see \url{http://api.census.gov/data/key_signup.html}.
}
\usage{
CensusAPI2010Spatial(variables, state.fips, 
level = c("county","tract","block group","block","cdp","congressional_district"), 
key, summaryfile = c("sf1", "ACS"), sp.object = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variables}{
This takes a character vector of SF1 IDs or ACS IDs, see \url{http://www.census.gov/prod/cen2010/doc/sf1.pdf} for SF1 and \url{http://www.census.gov/programs-surveys/acs/} for ACS variables.
}
  \item{state.fips}{ State fips for desired demographic variables.}
  \item{level}{ Takes "county", "tract", "block group", "block", or "cdp"}
  \item{key}{User provided key (see \url{http://api.census.gov/data/key_signup.html})}
  \item{summaryfile}{ "sf1" or "ACS"}
  \item{sp.object}{ sp object}
}
\details{
Add API Details.
}
\value{
data.frame object.
}
\references{
\url{http://api.census.gov/} \cr \cr

Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. 
}
\author{
Zack W. Almquist (almquist@umn.edu), Scott Chase (chase245@umn.edu)
}
\examples{
\dontrun{

key="YOUR KEY HERE"
sf1variables<-c("P0010001","P0030002",
"P0030003","P0030004","P0030005")
acsvariables<-c("B00001_001E",
"B01001_002E","B01001_026E","B06009_005E",
"B06009_005E","B19083_001E","B06011_002E")
#men, women, # bachelors deg, grad or prof deg, gini index, median income
test<-CensusAPI2010Spatial(sf1variables,state.fips="27",
level="tract",key,summaryfile="sf1")
test2<-CensusAPI2010Spatial(sf1variables,
state.fips=c("23","27"),level="county",key,summaryfile="sf1")


}

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
