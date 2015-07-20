\name{install.cdp}
\alias{install.cdp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Installer for the UScensus2010cdp package.
}
\description{
Convenient installer for the UScensus2010 package. Warning this installs from the source file for OS X and Linux and can take several minutes. Windows install only available for 2.11+ at this time.%Only 32 bit windows build available at this time.
}
\usage{
install.cdp()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{}{None}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\references{
Zack W. Almquist (2010). US Census Spatial and Demographic Data in R: The UScensus2000 Suite of Packages. Journal of Statistical Software, 37(6), 1-31. \url{http://www.jstatsoft.org/v37/i06/}. 
}
\author{
Zack W Almquist
}
\section{Warning }{
This is an extremely large file (around  180 megabytes) and should only be installed if you have a very good connection. Also it is worth noting that for all systems the install is from source and can take quite a bit of time to install.
} 

\examples{
\dontrun{
install.cdp("osx")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{install}

