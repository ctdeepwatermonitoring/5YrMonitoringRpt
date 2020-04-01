#new package install
install.packages("dplyr")
install.packages("stringr")
install.packages("stringi")
install.packages("tidyverse")
install.packages("lubridate")
-----------------------------
library('RSQLite')
setwd("C:/Users/kevin/Documents/Projects/GitHub/DEEP QA/5YrMonitoringRpt")
db_path <- paste0(getwd(),'/data/')
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));

SQL<- "SELECT *
       FROM 
          chemdata
       WHERE
          station_type='River/Stream' AND duplicate='0';"  
chem<-dbGetQuery(conn=db,SQL);

SQL<- "SELECT *
       FROM 
          mdl;"  
mdl<-dbGetQuery(conn=db,SQL);          

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

chem1<-chem[,c(1,3:6,11:12)]
chemMdl<-merge(chem1,mdl,by="chemparameter",all.x=TRUE)
chemMdl$valueMDL<-ifelse(is.na(chemMdl$value),chemMdl$MDL,chemMdl$value)

chem_basin<-merge(chem1,sitesbasin,by="sta_seq")

parametername<-chem[4]  
--------------------------
  #Parse chemparameter data:
  
  #parse out data by row for unique chem parameters
for (i in 1:length(chemunique)) {
  chem = chemunique[i]
  chem_total <-chem_basin[chem_basin$chemparameter==chem & chem_basin$duplicate==0, ]
}
---------------------------
##what chemical parameters were collected?
  
uniquechem<-unique(chem_basin$chemparameter)
ChemParam<-data.frame(uniquechem)

---------------------------
##How many river and stream sites were samples from 2011 through 2015? (edit)
  
NumberSites<-length(chem_basin$sta_seq)
Number_of_sites<-data.frame(NumberSites)

---------------------------
##How many samples were collected for each chemical parameter at river/stream sites? 
  
  #create vector of unique chem parameters
chemunique<-unique(chem_basin$chemparameter)

  #create empty dataframe to store chem parameter and count of samples
ChemSamp<-data.frame(chemparameter=character(),numsample=integer(),stringsAsFactors=FALSE)

  #loop for unique chem parameters
for (i in 1:length(chemunique)){
  #identify parameter(s)
  parameter.i<-chemunique[i]
  #count the # of samples 
  samplecount<-dim(chem_basin[chem_basin$chemparameter==parameter.i&chem_basin$duplicate==0,])[1]
  #create df w/ parameter(s) and count of sample 
  ChemSampParam<-data.frame(parameter.i,samplecount)
  #append df to empty df created outside of loop
  ChemSamp<-rbind(ChemSamp,ChemSampParam)
  
}
  #create df*
Parameter_Count<-data.frame(ChemSamp)

----------------------------------------
##Do all of the samples have the same unit of measure for each given parameter?
library(dplyr)
  #subset table 
uom_chemparameter<-chem_basin[,c("chemparameter","uom")]

  #number of distinct values
uom_match <- data.frame(uom_chemparameter %>%
                          gather(key = "fields",value = "Value") %>%
                          group_by(fields) %>%
                          summarise(distinct_value_count = n_distinct(Value, na.rm = TRUE))
)

  #extract unique rows from table
unique_uom <- data.frame(uom_chemparameter %>% distinct(chemparameter,uom,.keep_all = TRUE))

  #find common values (dplyr)
inner_join(uom_chemparameter, unique_uom)

  #identify rows in uom_chemparameter that are not present in unique_uom (dplyr)*

error_uom<-data.frame(setdiff(uom_chemparameter, unique_uom))

----------------------------------------
##How many river and stream samples were collected excluding duplicates?
  
  #exclude duplicates
nodup_chem<-distinct(chem_basin,.keep_all=TRUE)

Num_of_sample<-length(nodup_chem$station_type)

Num_riverstream<- data.frame(Num_of_sample)
----------------------------------------          
##How many river and stream samples (excluding duplicates) were collected in each year? (edit)
library(tidyverse)
  #parse out dates column
dates <- chem1[2]

  #create df for dates
dates_total <- data.frame(dates, stringsAsFactors = FALSE)

  #separate dates into "month", "day", "year"
monthdayyear<-data.frame(separate(dates_total,"collect_date", c("month", "day", "year"), sep = "/"))

  #parse out year sample was taken
sampleyear<-data.frame(monthdayyear[3])

  #vector of unique years samples were taken
uniqueyear<- unique(sampleyear$year)

  #create df of unique year and num of samples taken*
sampleyear.num<-data.frame(table(sampleyear))
----------------------------------------
##How many sites were collected in more than one year? ##What sites were collected in more than one year and how many years were collected?

  library(dplyr)
  #extract station sequence and collection date
sampleyear<-data.frame(chem_basin[1:2])
  #extract "year" from collection date
sampleyear1<-data.frame(separate(sampleyear,"collect_date", c("month", "day", "year"), sep = "/"))
  #subset df for station and year
sample_year<-subset(sampleyear1, select = c("sta_seq","year"))
  #remove duplicates 
sample_year_distinct<-distinct(sample_year)
  #create frequency table
sampleyear.freq<-table(sample_year_distinct)
print(sampleyear.freq)

  #convert table to dataframe (matrix)
sampleyear.df<-as.data.frame.matrix(sampleyear.freq)
  #sta_seq as new column
sampleyear.df<-cbind(sta_seq = row.names(sampleyear.freq), as.data.frame.matrix(sampleyear.freq))
  
  #add col w/ stations visited in more than one year
sampleyear.df$years.frequency<-rowSums(sampleyear.df[2:6]=="1")

  #filter out sites collected in more than one year & num of years collected*
more.than.one.sites<- sampleyear.df %>%
                          filter(years.frequency > 1)
  
  #number of sites sampled in more than one year*
length(more.than.one.sites$sta_seq)

------------------------------------------
##How many sites were collected in each major basin?

    #subset chem_basin
mbasn_basin<-subset(chem_basin,select = c("sta_seq","major"))
    #create table 
table.major.basin<-table(mbasn_basin)
    #sta_seq as new column
table.major.df<-cbind(sta_seq = row.names(table.major.basin), as.data.frame.matrix(table.major.basin))
    #sum of individual columns excluding sta_seq column
num.sites.per.mbasin<-colSums(table.major.basin[,-1])

major_basin_sitescount<-data.frame(num.sites.per.mbasin)
-------------------------------------------  

## What percentage of subregional basins in the State have one or more samples?

  
  
  
  
  