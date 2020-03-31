#new package install
install.packages("dplyr")  ##Once packages are installed you just need to use library() function
install.packages("stringr")
install.packages("stringi")  
install.packages("tidyverse")  
install.packages("lubridate")
-----------------------------
library('RSQLite')
setwd("/home/mkozlak/Documents/Projects/GitHub/5YrMonitoringRpt")
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

    #take unique chem parameters
chemunique<-unique(chem$chemparameter)

   
    #parse out data by row for unique chem parameters
for (i in 1:length(chemunique)) {
  chem = chemunique[i]
  chem_total <-chem_basin[chem_basin$chemparameter==chem & chem_basin$duplicate==0, ]
}

for (i in 1:length(chemunique)) {
  chemChoice <- chemunique[i]
  
    #create chem dataframe of specific chemical parameter
  parameter.i <- chem_basin[which(chem_basin$chemparameter == chemChoice), ]
}

---------------------------
#what chemical parameters were collected?

uniquechem<-unique(chem_basin$chemparameter)
ChemParam<-data.frame(uniquechem)

---------------------------
#How many river and stream sites were samples from 2011 through 2015? (edit)

NumberSites<-length(chem_basin$sta_seq)
Number_of_sites<-data.frame(NumberSites)

---------------------------
#How many samples were collected for each chemical parameter at river/stream sites? 
  
  #create empty dataframe that stores name & count of samples
  ChemSamp <- data.frame(chemparameter=character(), 
                         numsample=integer(), 
                         stringsAsFactors=FALSE)


    #create loop for unique chem parameters
    for (i in 1:length(chemunique)) {
      
      #create temp dataframe of specific chem parameter
      temp_chemcount <- chem_basin[which(chem_basin$chemparameter == chemunique[i]), ]
        
      #vector w/ number of samples 
      chemcount <- length(temp_chemcount$chemparameter)
        
      #dataframe of unique chem parameter and number of samples
      ChemP <- data.frame(chemunique[i], chemcount)
      
      #Rename to have the same column names as ChemSamp
      colnames(ChemP)<-c("chemparameter","numsample")
      
      #Store in ChemSamp dataframe
      ChemSamp<-rbind(ChemSamp,ChemP)
    }




#test code (ignore)
----------------------------------------

for (n in 1:nrow(parametername)) {
    samplenum<-
    print(paste(chemparameter[n]))
}


for (row in 1:nrow(chem_basin)) {
  chemparameter<- chem_basin[row, "Magnesium"]
  chemparameter<- chem_basin[row, "Magnesium"]
  chemparameter<-as.matrix(chemparameter)
  if(chem_basin==chemparameter) {
    print(paste("Magnesium", 
                "Nitrite","Nitrate","Ammonia","Calcium","Hardness"))
  }
}

----------------------------------------
#Do all of the samples have the same unit of measure for each given parameter?
  
    #subset table 
uom_chemparameter<-chem_basin[,c("chemparameter","uom")]

    #columns treated as vectors
(i in 1:ncol(uom_chemparameter)){uom_chemparameter[,i]=as.vector(uom_chemparameter[,i])})

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

    #identify rows in uom_chemparameter that are not present in unique_uom (dplyr)

error_uom<-data.frame(setdiff(uom_chemparameter, unique_uom))

----------------------------------------
#How many river and stream samples were collected excluding duplicates?

    #exclude duplicates
nodup_chem<-distinct(chem_basin,.keep_all=TRUE)

Num_of_sample<-length(nodup_chem$station_type)

Num_riverstream<- data.frame(Num_of_sample)
----------------------------------------          
#How many river and stream samples (excluding duplicates) were collected in each year?
    
    #parse out dates column
dates <- chem1[2]
    
    #create vector
datesvec<-(dates$collect_date)
    
    #create df for dates
dates_total <- data.frame(dates, stringsAsFactors = FALSE)
    
    #separate dates into "month", "day", "year"
monthdayyear<-data.frame(separate(dates_total,"collect_date", c("month", "day", "year"), sep = "/"))

    #parse out year sample was taken
sampleyear<-data.frame(monthdayyear[3])
    
    #vector of unique years samples were taken
uniqueyear<- unique(sampleyear$year)

    #create df of unique year and num of samples taken
sampleyear.df<-data.frame(table(sampleyear))
----------------------------------------
#How many sites were collected in more than one year? 

#What sites were collected in more than one year and how many years were collected?

#How many sites were collected in more than one year?

#what sites were collected in more than one year and how many years were collected?
