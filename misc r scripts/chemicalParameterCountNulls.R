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
#declare and set array/vector of chemical parameters
v_chems <- c("Chloride", "Hardness", "Total Nitrogen", "Total Phosphorus", "Turbidity", "Nitrite")
df_null <- NULL #declare and wipe data frame null clean for resuse
#loop until end of amound of chemical parameters
for (n in 1:length(v_chems)) {
  chem = v_chems[n]
  ##Parse out some data by row
  #df of all values
  n_total <-table_basin[table_basin$chemparameter==chem & table_basin$duplicate==0, ]
  #df of null values
  n_NA <-table_basin[table_basin$chemparameter==chem & table_basin$duplicate==0 & table_basin$value =='NA', ]
  
  #create list of values omiting NA
  l_isNumeric <- na.omit(n_total$value)
  l_isNumeric <- sapply(l_isNumeric, '[', seq(max(sapply(l_isNumeric, length))))
  
  df_nullTempRow <- data.frame(chemical = chem,
                               total = c(nrow(n_total)),
                               countNumeric = c(length(l_isNumeric)),
                               countNA = c(nrow(n_NA)))
  
  df_null <- rbind(df_null, df_nullTempRow)
}
#-------------------#

#display
df_null
#export table
write.csv(df_null, "S:/J_Tonfa/5YrMonitoringRpt/chemParameters/Results/nullCount.csv")