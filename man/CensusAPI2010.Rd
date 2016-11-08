\name{CensusAPI2010}
\alias{CensusAPI2010}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
US Census API function
}
\description{
Allows the user to access all Summary File 1 (SF1; 100-percent data; \url{http://www.census.gov/prod/cen2010/doc/sf1.pdf}) and American Community Survey (ACS; economic data). Requires the user to sign up for US Census API key, see \url{http://api.census.gov/data/key_signup.html}.
}
\usage{
CensusAPI2010(variables, state.fips, 
level = c("county","tract","block group","block","cdp","congressional_district"),
 key, summaryfile = c("sf1", "ACS"))
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
Scott Chase (chase245@umn.edu), Zack W. Almquist (almquist@umn.edu)
}
\examples{
\dontrun{

key="YOUR KEY HERE"
sf1variables<-c("P0010001","P0030002",
"P0030003","P0030004","P0030005")
acsvariables<-c("B00001_001E","B01001_002E",
"B01001_026E","B06009_005E",
"B06009_005E","B19083_001E","B06011_002E")
#men, women, # bachelors deg, grad or prof deg, gini index, median income
Twocounty<-CensusAPI2010(sf1variables,state.fips=c("23","27"),
level="county",key,summaryfile="sf1")



#cdp level works
mainecdp<-CensusAPI2010(sf1variables,
state.fips=c("23","27"),level="cdp",key,summaryfile="sf1")
mncdp<-CensusAPI2010(sf1variables,
state.fips="27",level="cdp",key,summaryfile="sf1")

#ACS level works
mnacs<-CensusAPI2010(acsvariables,
state.fips=c("27","23"),level="county",key,summaryfile="ACS")
maineacs<-CensusAPI2010(acsvariables,
state.fips="23",level="county",key,summaryfile="ACS")

#county level works
mainecounty<-CensusAPI2010(sf1variables,
state.fips="23",level="county",key,summaryfile="sf1")
Twocounty<-CensusAPI2010(sf1variables,
state.fips=c("23","27"),level="county",key,summaryfile="sf1")

mncounty<-CensusAPI2010(sf1variables,
state.fips="27",level="county",key,summaryfile="sf1")

#tract level works
mntract<-CensusAPI2010(sf1variables,
state.fips="27",level="tract",key,summaryfile="sf1")
mainetract<-CensusAPI2010(sf1variables,
state.fips="23",level="tract",key,summaryfile="sf1")

#block group level works
maineblkgrp<-CensusAPI2010(sf1variables,
state.fips="23",level="block group",key,summaryfile="sf1")
mnblkgrp<-CensusAPI2010(sf1variables,
state.fips="27",level="block group",key,summaryfile="sf1")

#block level works
maineblk<-CensusAPI2010(sf1variables,
state.fips="23",level="block",key,summaryfile="sf1")
mnblk<-CensusAPI2010(sf1variables,
state.fips="27",level="block",key,summaryfile="sf1")

}

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
