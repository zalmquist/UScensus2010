poly.clipper<-function(name, state, statefips = FALSE, level = c("tract","blk", "blkgrp"), sp.object = NULL, proj = NULL) 
{
poly.clipper.sub<-function (name, state, statefips = FALSE, level = c("tract","blk", "blkgrp"), sp.object = NULL, proj = NULL){ 
require(paste("UScensus2010", level, sep = ""), character.only = TRUE)
#require("UScensus2010cdp")
#require(rgeos)

city <- city(name, state, statefips)
state <- check.state(state, statefips)
        if (is.null(state)) {
            stop("Not a State! \n")
        }
        
 
        if (!is.null(sp.object)) {
            sp<- sp.object
        }else {
            x <- paste(state,".",level,"10", sep = "")
            utils::data(list = x, envir = parent.frame()) ###Check enviroment
            sp <- get(x)
        }

int<-gIntersection(sp,city,byid=TRUE)
int<-int@polyobj
int<-as(int,"SpatialPolygonsDataFrame")
int<-spChFIDs(int,sapply(strsplit(rownames(int@data)," "),"[[",1))
m<-match(rownames(int@data),rownames(sp@data))
int@data<-sp@data[m,]
phat<-areaPoly(int)/areaPoly(sp[m,])
phat[phat>1]<-1
int@data<-int@data[,!sapply(int@data,is.character)]*as.vector(phat)

if (is.null(proj) == FALSE) {
  #require(rgdal)
  requireNamespace("rgdal")
  int <- spTransform(int, proj)
}
int
}
poly.clipper.sub(name=name, state=state, statefips = statefips, level = level, sp.object = sp.object, proj = proj)
}