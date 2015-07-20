CensusAPI2010<-function(variables,state.fips,level=c("county","tract","block group","block","cdp"),key,summaryfile=c("sf1","ACS"))
	{
CensusData2010.sub<-function(variables,state.fips,level=c("county","tract","block group","block","cdp"),key,summaryfile=c("sf1","ACS"))
	{
		
	
		
		
	bf<-function(){	
	utils::data(fips2010,envir =parent.frame())
	assign("temp",fips2010)
	temp
	}
	fips2010<-bf()
	
	if(level!="county"&&summaryfile=="ACS")
		stop("ACS data only available at county level. Change level to county to continue.")
	#suppressMessages(require(rjson))
	level<-match.arg(level,several.ok=FALSE)
	summaryfile<-match.arg(summaryfile,several.ok=FALSE)
	#Make sure FIPS2010 is loaded.  This is setting up the FIPS.  Right now it is 3 MB.  Needs to be readily accessible.  How to get it on CRAN or on some server?
	if(level!="cdp")
		fips<-fips2010[,which(colnames(fips2010)==state.fips)]
	if(level=="cdp")
	{
		fips<-unique(fips2010[which(substr(fips2010[,52],1,2)==state.fips),"cdpfips"])
		fips.subset<-unique(substr(fips,3,7))
	}
	if(level=="county")
			fips<-unique(substr(fips,1,5))
	if(level=="tract")
			fips<-unique(substr(fips,1,11))
	if(level=="block group")
		{
			fips<-unique(substr(fips,1,12))
			fips<-fips[!is.na(fips)]
			#because we need to specify a county
			fips.subset<-unique(substr(fips,3,5))
		}
	if(level=="block")
		{
			fips<-unique(fips[!is.na(fips)])
			#because we need to specify a tract
			fips.subset<-unique(substr(fips,3,11))
		}

	#Clear out the NAs
	fips<-fips[!is.na(fips)]	
	
	APIcall<-function(fipscode)
	{
	if(level=="county"&&summaryfile=="sf1")
		url<-paste("http://api.census.gov/data/2010/",summaryfile,"?get=",gsub(", ",",",toString(variables)),"&for=county:*&in=state:",state.fips,"&key=",key,sep="")
		
	else if(level=="county"&&summaryfile=="ACS")
		url<-paste("http://api.census.gov/data/2010/acs5?get=",gsub(", ",",",toString(variables)),"&for=county:*&in=state:",state.fips,"&key=",key,sep="")
	
	else if(level =="tract")
		url<-paste("http://api.census.gov/data/2010/",summaryfile,"?get=",gsub(", ",",",toString(variables)),"&for=tract:*&in=state:",state.fips,"+county:",substr(fipscode,3,5),"&key=",key,sep="")
		
	else if(level =="block group")
		url<-paste("http://api.census.gov/data/2010/",summaryfile,"?get=",gsub(", ",",",toString(variables)),"&for=block+group:*&in=state:",state.fips,"+county:",fipscode,"&key=",key,sep="")
		
	else if(level =="block")
		url<-paste("http://api.census.gov/data/2010/",summaryfile,"?get=",gsub(", ",",",toString(variables)),"&for=block:*&in=state:",state.fips,"+county:",substr(fipscode,1,3),"+tract:",substr(fipscode,4,9),"&key=",key,sep="")
	
	else if(level=="cdp")
		url<-paste("http://api.census.gov/data/2010/",summaryfile,"?get=",gsub(", ",",",toString(variables)),"&for=place:*&in=state:",state.fips,"&key=",key,sep="")
		
	document <- rjson::fromJSON(file=url)
	m<-matrix(unlist(document),ncol=length(document[[1]]),byrow=TRUE)
	colnames(m)<-m[1,]
	m<-rbind(m[2:NROW(m),])
	m<-as.data.frame(m,stringsAsFactors=FALSE)
	return(m)
	}
	
	#Set up the blank matrix with an appropriate size
	d<-matrix(c(fips,rep(rep(NA,length(fips)),length(variables))),nrow=length(fips))
	d<-as.data.frame(d,stringsAsFactors=F)
	colnames(d)<-c("fips",c(variables))
	rownames(d)<-fips

#Accessing the API and putting the data in the right spot
	if(level=="county")
		{
			m<-APIcall(fipscode=state.fips)
			d[,2:(length(variables)+1)]<-m[match(paste(m$state,m$county,sep=""),rownames(d)),1:length(variables)]
		}
		
	if(level=="tract")
		{
		m<-APIcall(fipscode=state.fips)
		d[,2:(length(variables)+1)]<-m[match(paste(m$state,m$county,m$tract,sep=""),rownames(d)),1:length(variables)]
		}
		
	if(level=="block group")
		{
			for(j in 1:length(fips.subset))
			{
				m<-APIcall(fipscode=fips.subset[j])
			d[substr(d$fips,3,5)==fips.subset[j],2:(length(variables)+1)]<-m[match(paste(m$state,m$county,m$tract,m[,"block group"],sep=""),rownames(d[substr(d$fips,3,5)==fips.subset[j],])),1:length(variables)]
			}
		}
		
	if(level=="block")
	{
		for(j in 1:length(fips.subset))
		{
			m<-APIcall(fipscode=fips.subset[j])
			d[substr(d$fips,3,11)==fips.subset[j],2:(length(variables)+1)]<-m[match(paste(m$state,m$county,m$tract,m$block,sep=""),rownames(d[substr(d$fips,3,11)==fips.subset[j],2:(length(variables)+1)])),1:length(variables)]
		}
	}
	
	if(level=="cdp")
	{
		m<-APIcall(fipscode=state.fips)
		d[,2:(length(variables)+1)]<-m[match(paste(m$state,m$place,sep=""),rownames(d)),1:length(variables)]
	}
	
	for(k in 2:(NCOL(d))){d[,k]<-as.numeric(d[,k])}
	
	return(d)
}
### End of Scott's API function

### Wrapper to handle vectorization
wrapperCD2010<-function(variables,state.fips,level=c("county","tract","block group","block","cdp"),key,summaryfile=c("sf1","sf3"))
{

	if(length(state.fips)>1)
	{
		out<-lapply(state.fips,function(x){
	CensusData2010.sub(variables=variables,state.fips=x,level=level,key=key,summaryfile=summaryfile)
		})
		fout<-data.frame(matrix(NA,ncol=NCOL(out[[1]]),nrow=sum(sapply(out,NROW))))
		colnames(fout)<-colnames(out[[1]])
		rownames(fout)<-unlist(sapply(out,rownames))
		num<-sapply(out,NROW)
		num2<-c(1,num)
		num3<-cumsum(num2)
		
		for(i in 1:(length(num3)-1)){
		fout[seq(from=num3[i],to=num3[i+1]-1),]<-as.matrix(out[[i]])
		}
		for(k in 2:(NCOL(fout))){fout[,k]<-as.numeric(fout[,k])}
		return(fout)
	}
	
CensusData2010.sub(variables=variables,state.fips=state.fips,level=level,key=key,summaryfile=summaryfile)
}

### Final vector call
wrapperCD2010(variables=variables,state.fips=state.fips,level=level,key=key,summaryfile=summaryfile)
}