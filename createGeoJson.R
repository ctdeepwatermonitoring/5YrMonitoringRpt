library('RSQLite')
library(rgdal)

setwd("/home/mkozlak/Documents/Projects/GitHub/5YrMonitoringRpt")
db_path <- paste0(getwd(),'/data/')
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));

#Get site and chem data from the database
SQL<- "SELECT *
       FROM 
          chemdata
       WHERE
          station_type='River/Stream' AND duplicate='0';"  
chem<-dbGetQuery(conn=db,SQL);

SQL<-"SELECT
        sites.sta_seq,
        sites.name,
        sites.ylat,
        sites.xlong
      FROM 
        sites;"
sitesbasin<-dbGetQuery(conn=db,SQL)    

dbDisconnect(db);

##Get data together for the map
param<-c("Total Phosphorus","Total Nitrogen","Chloride","Turbidity")
chem<-chem[chem$chemparameter%in%param,]
chemAvg<-aggregate(value~sta_seq+chemparameter,chem,FUN=mean)
chemAvg<-reshape(chemAvg,idvar="sta_seq",timevar="chemparameter",direction="wide")
colnames(chemAvg)<-c("sta_seq","CL","TN","TP","TD")
sites<-sitesbasin[sitesbasin$sta_seq %in% unique(chem$sta_seq),c(1:4)]
sites<-merge(sites,chemAvg,by="sta_seq")
sites[is.na(sites)]<-0#for now fix later

##Create a geojson of the data
#Transform coordinates to numeric
sites$YLat  <- as.numeric(sites$ylat)
sites$XLong  <- as.numeric(sites$xlong)
sites.SP  <- SpatialPointsDataFrame(sites[,c(4,3)],
                                    sites[,-c(4,3)])
proj4string(sites.SP) <- CRS("+proj=utm +zone=18 +datum=WGS84") 
#proj4string(dataMap.SP) <- CRS("+init=epsg:4326") #WGS 84

str(sites.SP) # Now is class SpatialPointsDataFrame

#Write as geojson
writeOGR(sites.SP,"mapdata/sites.geojson",layer="sites", driver='GeoJSON',overwrite_layer = TRUE)