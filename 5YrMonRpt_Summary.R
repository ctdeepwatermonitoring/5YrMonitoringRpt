library('RSQLite')

#open ODBC
db_path <- 'S:/J_Tonfa/5YrMonitoringRpt/' 
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));

##Examples of how to run a sql statement in R
SQL<- "SELECT *
       FROM 
          chemdata
       WHERE
          station_type='River/Stream';"

table<-dbGetQuery(conn=db,SQL); #Executes the query above and puts data into a dataframe (R data structure)

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

dbDisconnect(db);

##Take a look at the first ten rows of the dataframe
table[1:10,]

##Get all unique chemistry parameters
unique(table$chemparameter)


##Get the column names of the dataframe
names(table)

##Subset a table by columns by specifing either the column name or column number
#table[,c("sta_seq","collect_date","chemparameter","value","uom","duplicate")]
table<-table[,c(1,3:6,11:12)]

#Join in R
table_basin<-merge(table,sitesbasin,by="sta_seq")

#Identify a data structure or data type
class(table_basin)
class(table_basin$value)

#Parse out some data by row
chloride<-table_basin[table_basin$chemparameter=="Chloride"&table_basin$duplicate==0,]

#Make sure all chem values have the same UOM & station type
unique(Chloride$uom)
unique(table$station_type)

#Get some summary statistics of a chem parameter
summary(chloride$value) #summary stats min, max, mean, median 
length(chloride$value) #total number of samples

#Get summary stats of a chem parameter by major basin and number of sample in each basin
aggregate(chloride["value"],by=list(MajorBasin=chloride$major),FUN=summary)
aggregate(chloride["value"],by=list(MajorBasin=chloride$major),FUN=summary)  
  
  
  
  

