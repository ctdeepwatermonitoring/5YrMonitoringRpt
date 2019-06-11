#The purpose of this script is to compare the 'totals' to the parts
#namely, we will compare Total Nitrogen to sum(Ammonia, Nitrite, Nitrate)
#and Total Phosphorus to Orthophosphorus
# Results will be organized by LAB

library('RSQLite')
#open ODBC
db_path <- 'S:/J_Tonfa/5YrMonitoringRpt/' 
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));

#query
SQL<- "select
  chemdata.lab_accession,
  chemdata.sta_seq,
  chemdata.chemparameter,
  chemdata.collect_date,
  chemdata.value,
  mdl.MDL
from sites
join
  chemdata on chemdata.sta_seq = sites.sta_seq
left join
  mdl on chemdata.chemparameter = mdl.chemparameter
;"

table <- dbGetQuery(conn=db, SQL)

dbDisconnect(db);

#replace lab_accession values with just the first six
#keep them as varchar type as by specification
table$lab_accession <- substring(table$lab_accession, 1, 6)

#create chem dataframe of specific chemical parameters
table <- table[table$chemparameter == 'Ammonia' |
                table$chemparameter == 'Nitrate' |
                table$chemparameter == 'Nitrite' |
                table$chemparameter == 'Ortho Phosphate' |
                table$chemparameter == 'Total Nitrogen' |
                table$chemparameter == 'Total Phosphorus', ]

#create array of distinct lab_accession values
labID <- unique(table$lab_accession)
labID <- as.numeric(labID)

#reset df_total
df_total = NULL

#loop through unique labIDs
for (n in 1:length(table$lab_accession)) {
  
  #declare temp chem from table values only from specific lab
  chem <- table[table$lab_accession == table$lab_accession[n], ]
  #chem <- table[table$lab_accession == 120156, ]
  
  #convert null values to 0
  chem$value[is.na(chem$value)] <- 0
  
  #aggregate sum by chemparameter
  chem_agg <- aggregate(chem$value, by=list(Category=chem$chemparameter), FUN=sum)
  
  #assign values
  ammonia <- chem_agg[1, 2]
  nitrate <- chem_agg[2, 2]
  nitrite <- chem_agg[3, 2]
  ortho   <- chem_agg[4, 2]
  tNitro  <- chem_agg[5, 2]
  tPhos   <- chem_agg[6, 2]
  
  sumDissolved <- ammonia + nitrate + nitrite
  
  df_table = data.frame(
    'LabID' = table$lab_accession[n],
    ammonia,
    nitrate,
    nitrite,
    'Sum of Dissolved' = sumDissolved,
    tNitro,
    'FLAG: D_sum > T_sum' = (sumDissolved > tNitro),
    'Ortho Phosphate' = ortho,
    'Total Phosphorus' = tPhos,
    'FLAG: Ortho > T_Phos' = (ortho > tPhos))
  
  #add to total dataframe
  df_total <- rbind(df_total, df_table)
}

#doublecheck: convert null phos values to 0
df_total$Total.Phosphorus[is.na(df_total$Total.Phosphorus)] <- 0
df_total$FLAG..Ortho...T_Phos[is.na(df_total$FLAG..Ortho...T_Phos)] <- FALSE

df_total <- unique(df_total)

file_chem <- paste("S:/J_Tonfa/5YrMonitoringRpt/chemParameters/Results/totalVsParts.csv", sep="")
write.csv(df_total, file_chem, row.names=FALSE)
