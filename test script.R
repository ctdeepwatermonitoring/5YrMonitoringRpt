
ChemP <- data.frame(chemparameter=character(), 
                numsample=integer(), 
                stringsAsFactors=FALSE)




chemunique<-unique(chem$chemparameter)

for (i in 1:length((chemunique1))) {
  parameter.i<-chem_basin[chem_basin$chemparameter==chemunique1[i]&chem_basin$duplicate==0,]
}


for (i in 1:length((chemunique))) {
  parameter.i<-chem_basin[chem_basin$chemparameter==chemunique[i]&chem_basin$duplicate==0,]
  print(parameter.i)
}


chemunique1 <- data.frame(chemunique, stringsAsFactors = FALSE)




chemunique<-unique(chem$chemparameter)
chemcount <- data.frame(chemparameter=character(), 
                    numsample=integer(), 
                    stringsAsFactors=FALSE)
for (i in 1:length(chemunique)) {
  
}
parameter.i<-chem_basin[chem_basin$chemparameter==chemunique[i]&chem_basin$duplicate==0,]



  
for (i in 1:length(chemunique)) {
  
  chemsampP<- data.frame(chemunique=character(),
                         numsample=integer(),
                         stringsAsFactors = FALSE)
  Chembind<-rbind(chemcount,chemsampP)
  print(Chembind)
}




for (i in 1:length((chemunique))) {
  parameter.i<-chem_basin[chem_basin$chemparameter==chemunique[i]&chem_basin$duplicate==0,]
  print(parameter.i)
}



for (i in 1:length(chemunique)) {
  chemcount <- (chemunique$Magnesium)
  
}








for (i in 1:length(chemunique)) {
  chem = chemunique[i]
  ##Parse out some data by row
  #df of all values
  chem_total <-chem_basin[chem_basin$chemparameter==chem & chem_basin$duplicate==0, ]
}


for (i in 1:length(chemunique)) {
  #Set specific chemical parameter
  chempara1 <- chemunique[i]
  #create df of specific chemical parameter
  chemi <- chem[which(chem$chemparameter == chempara1), ]
}






for (i in 1:length(chemunique)) {
  parameter.i<-chem1[chem1$chemparameter== chemunique[i]&chem1$duplicate==0,]
  print(chemunique[i])
}


parameter.i <- chem1 %>% filter(chemparameter == "Magnesium" | chemparameter == "Nitrate/Nitrite")


Ammonia <- chem1 %>% filter(chemparameter == "Ammonia")


for (i in 1:length(chemunique)) {
  
  #Set specific chemical parameter
  chempara <- chemunique[i]
  
  #create df of specific chemical parameter
  chemi <- chem[which(chem$chemparameter == chempara), ]
  ChemSampParam <- rbind(ChemSamp, chemi)
}

for (i in 1:length(chemunique)) {
  #Set chosen chemical
  chemChoice <- chemunique[i]
  #create chem dataframe of specific chemical parameter
  parameter.i <- chem_basin[which(chem_basin$chemparameter == chemChoice), ]
}



ChemP <- data.frame(chemparameter=character(), 
                    numsample=integer(), 
                    stringsAsFactors=FALSE)

for (i in 1:length(chemunique)) {
  #create temp dataframe of specific chem parameter
  temp_chemcount <- chem_basin[which(chem_basin$chemparameter == chemunique[i]), ]
  chemcount <- length(temp_chemcount$chemparameter)
  ChemP <- data.frame(chemunique[i], chemcount)
}


uom_chemparameter<-chem_basin[,c("chemparameter","uom")]


for (i in 1:length(chemunique)) {
  chemChoice <-chemunique[[i]]
  
  
}



uom_chemparameter %>% distinct(uom)




match(uom_chemparameter$chemparameter,uom_chemparameter$uom)


uom_match <- data.frame(uom_chemparameter %>%
                     gather(key = "chemparameter",value = "Value",uom) %>%
                     group_by(chemparameter) %>%
                     summarise(distinct_uom_count = n_distinct(Value, na.rm = TRUE))
)



uom_chemparameter %>% distinct(chemparameter,uom,.keep_all = TRUE)

unique_uom <- data.frame(uom_chemparameter %>% distinct(chemparameter,uom,.keep_all = TRUE))

inner_join(uom_chemparameter, unique_uom)
setdiff(uom_chemparameter, unique_uom)
