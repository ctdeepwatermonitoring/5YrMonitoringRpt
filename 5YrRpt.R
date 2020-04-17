library('RSQLite')
setwd("C:/Users/kevin/Documents/Projects/GitHub/DEEP QA/5YrMonitoringRpt")
env<-read.csv("data/site_env.csv",header=TRUE)
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
--------------------------
#vector of unique chemical parameters
uniquechem<-unique(chem_basin$chemparameter)
---------------------------
##what chemical parameters were collected?
  
uniquechem<-unique(chem_basin$chemparameter)
ChemParam<-data.frame(uniquechem)

---------------------------
##How many river and stream sites were samples from 2011 through 2015? (edit)
  
NumberSites<-length(chem_basin$collect_date)
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

  #extract unique rows from table
unique_uom <- 
  data.frame(uom_chemparameter %>% distinct(chemparameter,uom,.keep_all = TRUE))

  #find common values (dplyr)
inner_join(uom_chemparameter, unique_uom)

  #identify rows in uom_chemparameter that are not present in unique_uom (dplyr)*
error_uom<-data.frame(setdiff(uom_chemparameter, unique_uom))
----------------------------------------
##How many river and stream samples were collected excluding duplicates?
  #exclude duplicates
nodup_chem<-distinct(chem_basin,.keep_all=TRUE)
Num_of_sample<-data.frame(length(nodup_chem$station_type))
----------------------------------------          
##How many river and stream samples (excluding duplicates) were collected in each year? (edit)

  #create df for dates
library(tidyverse)
dates <- data.frame(chem_basin[2], stringsAsFactors = FALSE)

  #separate dates into "month", "day", "year"
monthdayyear<-data.frame(separate(dates,"collect_date", c("month", "day", "year"), sep = "/"))

  #create freq. table for sample years
sampleyearfreq<-as.data.frame(table(data.frame(monthdayyear[3])))
  colnames(sampleyear)[1] <- "Year"
----------------------------------------
##How many sites were collected in more than one year? ##What sites were collected in more than one year and how many years were collected?
require(tidyverse)
require(dplyr)
  
  #extract "year" from collection date
sampleyear<-
    data.frame(separate((chem_basin[1:2]),"collect_date", c("month", "day", "year"), sep = "/"))
  
  #subset df for station and year, remove duplicates
sample_year_distinct<-
  distinct(subset(sampleyear, select = c("sta_seq","year")))
  
  #create frequency table - dataframe, sta_seq as a new column
sampleyear.freq<-
  cbind(sta_seq = row.names(sampleyear.freq), 
        as.data.frame.matrix(table(sample_year_distinct)))
  
  #add col w/ stations visited in more than one year
sampleyear.freq$years.frequency<-rowSums(sampleyear.freq[2:6]=="1")
  
  #filter out sites collected in more than one year
morethanone<- sampleyear.freq %>%
  filter(years.frequency > 1)
  
  #number of sites sampled in more than one year*
length(morethanone$sta_seq)
------------------------------------------
##How many sites were collected in each major basin?
  #subset chem_basin
table.major.basin<-
  (table(subset(chem_basin,select = c("sta_seq","major"))))

  #sta_seq as new column
table.major.basin<-cbind(sta_seq = row.names(table.major.basin), 
                         as.data.frame.matrix(table.major.basin))

  #sum, exclude sta_seq
sites.per.mbasin<-data.frame(colSums(table.major.basin[,-1]))
  colnames(sites.per.mbasin)[1] <- "Site Frequency"
-------------------------------------------  
## What percentage of subregional basins in the State have one or more samples? (edit)
    require(dplyr)
  #create frequency table
sbasn<-table(subset(chem_basin, select = c("sbasn", "sta_seq")))
  #sbasn as column 1, convert to matrix df
sbasn.total<-cbind(sbasn = row.names(sbasn), as.data.frame.matrix(sbasn))
  #add sum of row as last column
sbasn.total<-data.frame(cbind(sbasn.total, total = rowSums(sbasn.total[-1])))
sbasn.sample.frequency<-count(sbasn.total, total > 1)
  
"percentage of subregional basins with more than one sample"<-print(100 * (sbasn.sample.frequency$n / length(unique(sbasn.total$sbasn))))     
---------------------------------------------
##What are the summary statistics for each parameter? ##Only calculate summary statistics for river/stream samples and non-duplicates
  #Create a summary stats dataframe
summary_Stats<-data.frame(param=character(),Min=numeric(),Q1 =numeric(),Median=numeric(),
                          Mean=numeric(),Q3 =numeric(),
                          Max=numeric(),NAs=integer())

for (i in 1:length(uniquechem)){
  param<-chem_basin[chem_basin$chemparameter==uniquechem[i]&chem_basin$duplicate==0,4]
  cntNA<-length(param[is.na(param)])
  test<-(summary(param)[1:6])
  test<-as.data.frame(as.matrix(test))
  test<-data.frame(param=uniquechem[i],Min=test[1,],Q1=test[2,],Median=test[3,],
                   Mean=test[4,],Q3=test[5,],Max=test[6,],NAs=cntNA)
  summary_Stats<-rbind(test,summary_Stats)
}  
---------------------------------------------  
##What are the summary statistics for each parameter in each major basin? (edit)
  #aggregate values based on major basin & parameter
mbasin.para<-aggregate(chem_basin$value , by = list(chem_basin$major , chem_basin$chemparameter )  , FUN = summary)  

require(dplyr)

  #rename columns
mbasin.para<- mbasin.para %>% 
  dplyr::rename(
    MajorBasin = Group.1,
    Chemparameter = Group.2,
    SummaryStats = x
  )
  #sort columns by major basin
mbasin.para<-data.frame(arrange(mbasin.para,MajorBasin))
---------------------------------------------  
##what are the summary stats for drainage area for the sites? By major basin?
  
require(dplyr)

StationMajor<-
  data.frame(subset(distinct(chem_basin,sta_seq, .keep_all = TRUE), 
                    select = c("sta_seq","major")))

#ID mismatch*
anti_join(env,StationMajor)

Majorenv<-
  subset(merge(StationMajor[StationMajor$sta_seq!=14302,],env,by="sta_seq"), 
         select = c("major","SqMi"))

#summary stats drainage area (miles)
summary(Majorenv$SqMi)

#summary stats drainage area (miles) by major basin
Drainagearea<-as.data.frame(Majorenv %>% 
                              group_by(major) %>%
                              summarise_at(.vars = names(.)[2],.funs = c(min = "min", 
                                                                         q1 = ~quantile(.,probs = 0.25), 
                                                                         median = "median", 
                                                                         q2 = ~quantile(.,probs = 0.75), 
                                                                         max = "max",
                                                                         mean = "mean", 
                                                                         sd = "sd")))
----------------------------------------------
##What are the summary stats for percent impervious cover for the sites? By major basin?

  #summary stats percent impervious cover
summary(Majorenv$IC_Avg)

Majorenv.IC<-subset(merge(StationMajor[StationMajor$sta_seq!=14302,],env,by="sta_seq"), 
                    select = c("major","IC_Avg"))

  #summary stats percent impervious cover by major basin
PercentIC<-as.data.frame(Majorenv.IC %>% 
                              group_by(major) %>%
                              summarise_at(.vars = names(.)[2],.funs = c(min = "min", 
                                                                         q1 = ~quantile(.,probs = 0.25), 
                                                                         median = "median", 
                                                                         q2 = ~quantile(.,probs = 0.75), 
                                                                         max = "max",
                                                                         mean = "mean", 
                                                                         sd = "sd")))
