library('RSQLite')

#The purpose of this script is to flag duplicate pairs that fall outside
#of a predefined acceptable range in terms of difference
#Uses field replicate percentage

#open ODBC
db_path <- 'S:/J_Tonfa/5YrMonitoringRpt/' 
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));

#SQL query for duplicates using following criteria
#same: chemparameter, sta_seq, collection date, duplicate (0 to 1)
#Create dataframe of duplicates
#note: assume chem dataframe already has desired chemparameter
SQL <- "select
c.sta_seq,
c.chemparameter,
c.collect_date,
c.value as 'value1',
c2.value as 'value2',
c.uom
from chemdata c, chemdata c2
where
c.sta_seq = c2.sta_seq
and c.collect_date = c2.collect_date
and c.chemparameter = c2.chemparameter
and (c.duplicate + c2.duplicate) = 1 --check that there is only one duplicate
and c.station_type='River/Stream'
and c2.station_type='River/Stream'
;"

table <- dbGetQuery(conn=db, SQL)

#retrieve chemical quality assurance table
SQL <- "select *
from chemQA"

table_QA <- dbGetQuery(conn=db, SQL)

#get mdl (minimum detection limit) table
SQL <- "SELECT *
FROM mdl;"

table_mdl <- dbGetQuery(conn=db, SQL)

dbDisconnect(db);


#--- LOOP CONCEPT ---#
#declare and set array/vector of chemical parameters (enter any valid chemical you want)
v_chems <- c("Nitrite", "Total Nitrogen", "Total Phosphorus")
df_total <- NULL

#loop until end of amound of chemical parameters
for (n in 1:length(v_chems)) {
  #Set chosen chemical
  chemChoice <- v_chems[n]
  chemChoice <- 'Nitrite'
  #create chem dataframe of specific chemical parameter
  chem <- table[table$chemparameter == chemChoice, ]
  
  #Create MDL table
  test_table_mdl <- table_mdl[table_mdl$chemparameter == chemChoice &
                                table_mdl$MDL, ]
  mdl <- test_table_mdl[1, 2] # convert value to MDL numeric vector
  
  #get field duplicate precision rate for chemparameter
  RPD <- table_QA$field_duplicate_precision[table_QA$chemparameter == chemChoice]
  
  ###cbind count of values below mdl
  #head(chem)
  #df_test <- NULL
  #df_test <- c("# of Values Below MDL" = sum(is.na(chem$value1)) + sum(is.na(chem$value2)))
  
  #df_test <- addmargins(chem,
  #                      FUN = list(Total = sum(is.na(chem$value1)) + sum(is.na(chem$value2)),
  #                                 quiet = TRUE))
  
  #("# of Values Below MDL" = rowSums(is.na(chem$value1)) + sum(is.na(chem$value2)))
  #rowsum(chem, value1)
  ###this is really hard
  
  #convert all NULL or N/A values to chemical's MDL (minimum detection limit)
  chem$value1[is.na(chem$value1)] <- mdl
  chem$value2[is.na(chem$value2)] <- mdl
  
  #add column/field to mark if pair failed RPD test
  chem <- cbind(chem, "RPD_test" = FALSE)
  
  #if percentage is less than RPD, pass = 'TRUE'
  chem$RPD_test <- (abs((chem$value2 - chem$value1) / chem$value1)) < RPD
  
  #add to total dataframe
  df_total <- rbind(df_total, chem)
}
#-----END-LOOP------#

#concatenate filename and destination, sep="" for no whitespace
file_chem <- paste("S:/J_Tonfa/5YrMonitoringRpt/chemParameters/Results/duplicatePairs.csv", sep="")
#export table
write.csv(df_total, file_chem, row.names=FALSE)
