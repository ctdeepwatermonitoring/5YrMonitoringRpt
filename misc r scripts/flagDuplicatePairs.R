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
SQL <- "
select distinct
  c.sta_seq,
  c.collect_date,
  c.chemparameter,
  c.value as 'value_field',
  c2.value as 'value_duplicate'
from chemdata c, chemdata c2
where
  c.sta_seq = c2.sta_seq
  and c.collect_date = c2.collect_date
  and c.chemparameter = c2.chemparameter
  and c.duplicate = 0
  and c2.duplicate = 1
  and c.station_type='River/Stream'
  and c2.station_type='River/Stream'
group by c.sta_seq, c.chemparameter, c.collect_date
;"

table_pairs <- dbGetQuery(conn=db, SQL)

#retrieve chemical quality assurance table
SQL <- "select *
from chemQA"

table_QA <- dbGetQuery(conn=db, SQL)

#get mdl (minimum detection limit) table
SQL <- "SELECT *
FROM mdl;"

table_mdl <- dbGetQuery(conn=db, SQL)

dbDisconnect(db);

table_combined <- table_pairs


#--- LOOP CONCEPT ---#
#declare and set array/vector of chemical parameters (enter any valid chemical you want)
v_chems <- c("Ammonia", "Nitrate", "Nitrite", "Total Nitrogen", "Total Phosphorus")
df_total <- NULL #reset df_total at runtime

#loop until end of amound of chemical parameters
for (n in 1:length(v_chems)) {
  #Set chosen chemical
  chemChoice <- v_chems[n]
  #create chem dataframe of specific chemical parameter
  chem <- table_combined[table_combined$chemparameter == chemChoice, ]
  
  #Create MDL table
  test_table_mdl <- table_mdl[table_mdl$chemparameter == chemChoice & table_mdl$MDL, ]
  mdl <- test_table_mdl[1, 2] # convert value to MDL numeric vector
  
  #get FIELD duplicate precision rate for chemparameter
  RPD <- table_QA$field_duplicate_precision[table_QA$chemparameter == chemChoice]
  
  #FLAG: values below MDL
  chem <- cbind(chem, 'FLAG: value_field < mdl BEFORE conversion' =
                  (is.na(chem$value_field)))
  chem <- cbind(chem, 'FLAG: value_duplicate < mdl BEFORE conversion' =
                  (is.na(chem$value_duplicate)))
  
  #convert all NULL or N/A values to chemical's MDL (minimum detection limit)
  chem$value_field[is.na(chem$value_field)] <- mdl
  chem$value_duplicate[is.na(chem$value_duplicate)] <- mdl
  
  #add columns/fields to mark if pair failed RPD test
  chem <- cbind(chem, 'field_precision_rate' = RPD)
  chem <- cbind(chem, "Percent_Diff" = 0.0)
  
  #percent difference
  chem$Percent_Diff <- c(
    abs(chem$value_field - chem$value_duplicate) /
      ((chem$value_field + chem$value_duplicate) / 2)
  )
  
  #add MDL column to chem df
  chem <- cbind(chem, 'mdl' = mdl)
  
  #add FLAG column
  #TRUE if value_field value$field < mdl
  chem <- cbind(chem, 'FLAG: value_field < mdl' =
                  (chem$value_field < chem$mdl))
  #another FLAG of TRUE if Percent_Diff > RPD (30%)
  chem <- cbind(chem, 'FLAG: Percent_Diff > RPD' =
                  (chem$Percent_Diff > chem$field_precision_rate))
  
  #add chem df to total dataframe
  df_total <- rbind(df_total, chem)
}
#-----END-LOOP------#

#need to convert $collect_date to date format
df_total$collect_date <- as.Date(df_total$collect_date, "%m/%d/%Y")
# sort
df_total <- df_total[with(df_total, order(chemparameter, collect_date, sta_seq)), ]
#convert to percent format for viewing
df_total$Percent_Diff <- paste(df_total$Percent_Diff * 100, "%")
df_total$field_precision_rate <- paste(df_total$field_precision_rate * 100, "%")

#reorder based on index
#df_total <- df_total[c(1:4, 6, 5, 7:12)]

#concatenate filename and destination, sep="" for no whitespace
file_chem <- paste("S:/J_Tonfa/5YrMonitoringRpt/chemParameters/Results/duplicatePairs.csv", sep="")
#export table
write.csv(df_total, file_chem, row.names=FALSE)
