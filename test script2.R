
-------------

Magnesium<-chem_basin[chem_basin$chemparameter=="Magnesium"&chem_basin$duplicate==0,]
Nitrite<-chem_basin[chem_basin$chemparameter=="Nitrite"&chem_basin$duplicate==0,]
Nitrate<-chem_basin[chem_basin$chemparameter=="Nitrate"&chem_basin$duplicate==0,]
Ammonia<-chem_basin[chem_basin$chemparameter=="Ammonia"&chem_basin$duplicate==0,]
Calcium<-chem_basin[chem_basin$chemparameter=="Calcium"&chem_basin$duplicate==0,]
Hardness<-chem_basin[chem_basin$chemparameter=="Hardness"&chem_basin$duplicate==0,]
"Organic Nitrogen"<-chem_basin[chem_basin$chemparameter=="Organic Nitrogen"&chem_basin$duplicate==0,]
"Ortho Phosphate"<-chem_basin[chem_basin$chemparameter=="Ortho Phosphate"&chem_basin$duplicate==0,]
"pH"<-chem_basin[chem_basin$chemparameter=="pH"&chem_basin$duplicate==0,]
"Total Kjeldahl Nitrogen"<-chem_basin[chem_basin$chemparameter=="Total Kjeldahl Nitrogen"&chem_basin$duplicate==0,]
"Total Nitrogen"<-chem_basin[chem_basin$chemparameter=="Total Nitrogen"&chem_basin$duplicate==0,]
"Total Solids"<-chem_basin[chem_basin$chemparameter=="Total Solids"&chem_basin$duplicate==0,]
"Total Suspended Solids"<-chem_basin[chem_basin$chemparameter=="Total Suspended Solids"&chem_basin$duplicate==0,]
Turbidity<-chem_basin[chem_basin$chemparameter=="Turbidity"&chem_basin$duplicate==0,]
Alkalinity<-chem_basin[chem_basin$chemparameter=="Alkalinity"&chem_basin$duplicate==0,]
"Total Phosphorous"<-chem_basin[chem_basin$chemparameter=="Total Phosphorous"&chem_basin$duplicate==0,]
Chloride<-chem_basin[chem_basin$chemparameter=="Chloride"&chem_basin$duplicate==0,]
"Total Organic Carbon"<-chem_basin[chem_basin$chemparameter=="Total Organic Carbon"&chem_basin$duplicate==0,]
Silica<-chem_basin[chem_basin$chemparameter=="Silica"&chem_basin$duplicate==0,]
"Chlorophyll-a Plankton"<-chem_basin[chem_basin$chemparameter=="Chlorophyll-a Plankton"&chem_basin$duplicate==0,]
"Nitrate/Nitrite"<-chem_basin[chem_basin$chemparameter=="Nitrate/Nitrite"&chem_basin$duplicate==0,]

#create list that stores dfs
list_parameters<-list(Magnesium, 
                      Nitrate, 
                      Nitrite, 
                      Ammonia, 
                      Calcium, 
                      Hardness, 
                      `Organic Nitrogen`,
                      `Ortho Phosphate`,
                      pH,
                      `Total Kjeldahl Nitrogen`,
                      `Total Nitrogen`,
                      `Total Solids`,
                      `Total Suspended Solids`,
                      Turbidity,
                      Alkalinity,
                      `Total Phosphorous`,
                      Chloride,
                      `Total Organic Carbon`,
                      Silica,
                      `Chlorophyll-a Plankton`,
                      `Nitrate/Nitrite`)


#convert list into data frame
DT.parameters <- rbindlist(list_parameters)
#subset for parameter name and value
DT.parameters <-subset(DT.parameters, select = c("chemparameter","value"))

chemunique
col<-match(DT.parameters$chemparameter, chemunique)
row<-cumsum(c(0,diff(col))<=0)

Param_summ <- matrix(nrow=max(row), ncol=max(col))
colnames(Param_summ) <- chemunique
Param_summ[cbind(row, col)] <- DT.parameters$value



library(pastecs)
options(scipen = 100)
options(digits = 2)
stat.desc(Param_summ)