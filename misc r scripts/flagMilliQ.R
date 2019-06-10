library('RSQLite')
#The purpose of this script is to flag duplicate pairs that fall outside
#of a predefined acceptable range in terms of difference
#Uses field replicate percentage

#open ODBC
db_path <- 'S:/J_Tonfa/5YrMonitoringRpt/' 
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));

#get table of Milli-Q values, identified by chemparameter and collection date
SQL <- "
select
  chemdata.lab_accession,
  chemdata.sta_seq,
  chemdata.chemparameter,
  chemdata.collect_date,
  chemdata.value as 'Milli_Q',
  mdl.MDL
from sites
join
  chemdata on chemdata.sta_seq = sites.sta_seq
left join
  mdl on chemdata.chemparameter = mdl.chemparameter
where sites.name like '%milli%'
;"

table_milliQ <- dbGetQuery(conn=db, SQL)

#retrieve chemical quality assurance table
SQL <- "select *
from chemQA"

table_QA <- dbGetQuery(conn=db, SQL)

dbDisconnect(db);


#--- LOOP CONCEPT ---#
#declare and set array/vector of chemical parameters (enter any valid chemical you want)
v_chems <- c("Ammonia", "Nitrate", "Nitrite", "Total Nitrogen", "Total Phosphorus")
df_total <- NULL #reset df_total at runtime

#loop until end of amound of chemical parameters
for (n in 1:length(v_chems)) {
  #Set chosen chemical
  chemChoice <- v_chems[n]
  #create chem dataframe of specific chemical parameter
  chem <- table_milliQ[table_milliQ$chemparameter == chemChoice, ]
  
  #get lab duplicate precision rate for chemparameter
  RPD <- table_QA$lab_duplicate_precision[table_QA$chemparameter == chemChoice]
  
  #FLAG if milliQ was null before conversion
  chem <- cbind(chem, 'FLAG: milli-Q NULL BEFORE conversion' =
                  (is.na(chem$Milli_Q)))
  
  #convert null milliQ values to mdl
  mdl <- chem$MDL[1]
  chem$Milli_Q[is.na(chem$Milli_Q)] <- mdl
  
  #add columns/fields to mark if pair failed RPD test
  chem <- cbind(chem, 'lab_precision_rate' = RPD)
  chem <- cbind(chem, "Percent_Diff" = 0.0)
  
  #percent difference
  chem$Percent_Diff <- c(
    abs(chem$Milli_Q - chem$MDL) /
      ((chem$Milli_Q + chem$MDL) / 2)
  )
  
  #add FLAG column
  #TRUE if Percent_Diff is greater than RPD (15%)
  chem <- cbind(chem, 'FLAG: Percent_Diff > RPD' =
                  (chem$Percent_Diff > chem$lab_precision_rate))
  
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
df_total$lab_precision_rate <- paste(df_total$lab_precision_rate * 100, "%")

#concatenate filename and destination, sep="" for no whitespace
file_chem <- paste("S:/J_Tonfa/5YrMonitoringRpt/chemParameters/Results/milliQ.csv", sep="")
#export table
write.csv(df_total, file_chem, row.names=FALSE)

###count of values below mdl
# df_test <- NULL
# df_test <- cbind(chem, 'below_mdl_count' = 0)
# head(df_test)
# for (i in 1:length(chem)) {
#   if (is.na(chem$value2[i]) == TRUE) {
#     df_test$below_mdl_count[i] <- df_test$below_mdl_count[i] + 1
#   }
#   if (is.na(chem$value1[i]) == TRUE) {
#     df_test$below_mdl_count[i] <- df_test$below_mdl_count[i] + 1
#   }
# }
# #add column for below_mdl_count
# chem <- cbind(chem, 'Values Below MDL' = df_test$below_mdl_count)
