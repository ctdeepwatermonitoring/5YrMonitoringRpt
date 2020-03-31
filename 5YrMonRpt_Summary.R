#Set working directory to your local repository
setwd("/home/mkozlak/Documents/Projects/GitHub/5YrMonitoringRpt")

#Load the SQLite library
library('RSQLite')

#Load external csv files.  This is landscape information that was previously process for each site
env<-read.csv("data/site_env.csv",header=TRUE)

#ODBC (Connect to the database)
db_path <- '/home/mkozlak/Documents/Projects/GitHub/5YrMonitoringRpt/data/' #Change this local DB directory
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));

#####Examples of how to run a sql statement in R############################################################
############################################################################################################

##Selects all data from chemdata where a sample was taken in a river and stream and was not a field duplicate
SQL<- "SELECT *
       FROM 
          chemdata
       WHERE
          station_type='River/Stream' AND duplicate='0';"  

chem<-dbGetQuery(conn=db,SQL); #Executes the query above and puts data into a dataframe (R data structure)

SQL<- "SELECT *
       FROM 
          mdl;"  

mdl<-dbGetQuery(conn=db,SQL); #Executes the query above and gets the mdl for chem parameters where available

##Example of a join.  Joins information in the sites table with information in the basin table
SQL<-"SELECT
        sites.sta_seq,
        sites.name,
        sites.ylat,
        sites.xlong,
        sites.sbasn,
        basin.major,
        basin.mbasn
      FROM 
        sites
      JOIN 
        basin
      ON
        sites.sbasn = basin.sbasn;"

sitesbasin<-dbGetQuery(conn=db,SQL)

dbDisconnect(db);  #Make sure to disconnect from the DB when done running queries

#####Examples of how to manipulate data in R################################################################
############################################################################################################

##Take a look at the first ten rows of the dataframe
chem[1:10,]

##Get all unique chemistry parameters
unique(chem$chemparameter)
length(unique(chem$chemparameter))#Count all unique chem parameters


##Get the column names of the dataframe
names(chem)

##Subset a table by columns by specifing either the column name or column number
#chem[,c("sta_seq","collect_date","chemparameter","value","uom","duplicate")]
chem<-chem[,c(1,3:6,11:12)]
chemMdl<-merge(chem,mdl,by="chemparameter",all.x=TRUE)#merge chem with available MDL values
##If a chem value is NA (i.e. below detection limit use mdl)
chemMdl$valueMDL<-ifelse(is.na(chemMdl$value),chemMdl$MDL,chemMdl$value)

#Join in base R
chem_basin<-merge(chem,sitesbasin,by="sta_seq")

#Identify a data structure (dataframe, list, matrix etc) or data type(numeric, string, etc.)
class(chem_basin)
class(chem_basin$value)

#Parse out some data by row
chloride<-chem_basin[chem_basin$chemparameter=="Chloride"&chem_basin$duplicate==0,]

#Make sure all chem values have the same UOM & station type
unique(Chloride$uom)
unique(chem$station_type)

#Get some summary statistics of a chem parameter
summary(chloride$value) #summary stats min, max, mean, median 
mean(chloride$value)# just the mean value
length(chloride$value) #total number of samples

#Get summary stats of a chem parameter by major basin and number of sample in each basin
aggregate(chloride["value"],by=list(MajorBasin=chloride$major),FUN=summary)
aggregate(chloride["value"],by=list(MajorBasin=chloride$major),FUN=summary)  

#####Questions to help us pull together information for the report###########################################
#####Using examples above and tutorial work, put it all together to write a script to answer questions below#
#############################################################################################################
  
#How many river and stream sites were samples from 2011 through 2015?

#What chemical parameters were collected?

ChemP<-unique(chem$chemparameter)

#How many samples were collected for each chemical parameter at river/stream sites?

ChemSamp<-data.frame(parameter=character(),paramCnt=integer())

for (i in 1:length(ChemP)){
  parameter<-ChemP[i]
  paramCnt<-dim(chem_basin[chem_basin$chemparameter==parameter&chem_basin$duplicate==0,])[1]
  ChemSampParam<-data.frame(parameter,paramCnt)
  ChemSamp<-rbind(ChemSamp,ChemSampParam)
  
}

#Do all of the samples have the same unit of measure for each given parameter?

#How many river and stream samples were collected excluding duplicates?

#How many river and stream samples (excluding dups) were collected in each year?

#How many sites were collected in more than one year?

#What sites were collected in more than one year and how many years were collected?

#How many sites were collected in each major basin?

#What percentage of subregional basins in the State have one or more samples? 

#What are the summary statistics for each parameter
#Only calculate summary statistics for river/stream samples and non-duplicates

#What are the summary statistics for each parameter in each major basin?

#What are the summary stats for drainage area for the sites?  By major basin?

#What are the summary stats for percent impervious cover for the sites?  By major basin? 

  
  

