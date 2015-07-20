CensusAPI2010Spatial<-function(variables, state.fips, level = c("county", "tract", 
    "block group", "block", "cdp"), key, summaryfile = c("sf1","ACS"), sp.object=NULL){

CensusAPI2010Spatial.sub<-function(variables, state.fips, level = c("county", "tract", 
    "block group", "block", "cdp"), key, summaryfile = c("sf1","ACS"), sp.object=NULL){

    	demographics<-CensusAPI2010(variables,state.fips,level,key,summaryfile)
    	

    state <- sapply(state.fips,check.state,statefips=TRUE)
    if (is.null(state)) {
        stop("Not a State! \n")
    }	
    
    
    if (!is.null(sp.object)) {
       temp <- sp.object
    }else {
    	##require data package
    require(paste("UScensus2010", level, sep = ""), character.only = TRUE)
    x <- paste(state, ".", level, "10", sep = "")
    data(list = x, envir = parent.frame())
    temp <- lapply(x,get,envir =parent.frame())
    
    if(length(temp)==1){
    	temp<-temp[[1]]
    	}else{
    	temp2<-temp[[1]]
    	for(i in 2:length(temp)){
    	temp2<-spRbind(temp2,temp[[i]])
    	}
    	temp<-temp2
    }
    
    
    
    }
    
    
    data<-temp@data
    m<-match(data$fips,	demographics$fips)
	data<-cbind(data,demographics[m,])
	temp@data<-data
	temp
}
CensusAPI2010Spatial.sub(variables=variables, state.fips=state.fips, level=level, key=key, summaryfile=summaryfile, sp.object=sp.object)
} ## End of CensusAPI2010Spatial