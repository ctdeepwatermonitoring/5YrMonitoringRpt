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

#Join in R syntax to create a table with sites and chemicals
table_basin<-merge(table,sitesbasin,by="sta_seq")

#--- LOOP CONCEPT ---#
#declare and set array/vector of chemical parameters (enter any valid chemical you want)
v_chems <- c("Chloride", "Hardness", "Total Nitrogen", "Total Phosphorus", "Turbidity", "Nitrite")

#loop until end of amound of chemical parameters
for (n in 1:length(v_chems)) {
  #create chem dataframe
  chem <- table_basin[table_basin$chemparameter == v_chems[n] & table_basin$duplicate==0
                      & table_basin$value != 'NULL', ]
  
  #check that unit of measurement is same throughout
  #unique(chem$uom)
  #unique(chem$duplicate) # check for no duplicate flags i.e. 1
  
  #aggregate dataframe by major basin name levels/categories
  agg_chem <- aggregate(chem["value"], by=list(MajorBasin = chem$major), FUN=summary)
  
  #create vector with count of samples (total and by basin)
  countByBasn <- aggregate(chem["value"], by=list(MajorBasin=chem$major), FUN=length)
  countByBasn <- countByBasn[, 2] #keep only the second column
  agg_chem <- cbind(agg_chem, "# of Samples" = countByBasn) #add column
  
  #create new row of total
  totalRow <- aggregate(chem["value"], by=list(MajorBasin=chem$chemparameter), FUN=summary)
  totalRow$MajorBasin[1] <- "Total" #assign 'basin' name as 'Total'
  totalRow <- cbind(totalRow, "# of Samples" = length(chem$value)) #add total # of samples column
  
  agg_chem <- rbind(agg_chem, totalRow) # add "Total" row to form final data frame
  
  #display
  agg_chem
  #concatenate filename and destination, sep="" for no whitespace
  file_chem <- paste("S:/J_Tonfa/5YrMonitoringRpt/chemParameters/Results/", as.character(v_chems[n]), "-data.csv", sep="")
  #export table
  write.csv(agg_chem, file_chem)
}
#-------------------#