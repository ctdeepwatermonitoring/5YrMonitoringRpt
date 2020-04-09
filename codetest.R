chemunique<-unique(chem_basin$chemparameter)


ChemSamp<-data.frame(chemparameter=character(),numsample=integer(),stringsAsFactors=FALSE)

for (i in 1:length(chemunique)){
  parameter.i<-chemunique[i]
  samplecount<-dim(chem_basin[chem_basin$chemparameter==parameter.i&chem_basin$duplicate==0,])[1]
  ChemSampParam<-data.frame(parameter.i,samplecount)
  ChemSamp<-rbind(ChemSamp,ChemSampParam)
  
}

Parameter_Count<-data.frame(ChemSamp)





##How many sites were collected in more than one year? 

#unique sta_seq
sta.seq.unique<-unique(chem_basin$sta_seq)

#subset chem_basin for sta_seq
staseq<-chem_basin[1]
Site.year.frequency<-data.frame(sites=integer(),year=integer(),stringsAsFactors = FALSE)

for (i in 1:length(sta.seq.unique)){
  staseq.i<-sta.seq.unique[i]
  sampleyear<-data.frame(monthdayyear[3])
  staseq1<-data.frame(staseq.i,sampleyear)
  Site.year.frequency<-rbind(Site.year.frequency,staseq1)
}

Site.year.frequency1<-data.frame(distinct(Site.year.frequency, staseq.i,.keep_all = TRUE))



library(dplyr)

Site.year.frequency %>% group_by(staseq.i,year) %>%
  summarise(count=(number=2011))


---------------------------
sampleyear<-data.frame(chem_basin[1:2])
sampleyear1<-data.frame(separate(sampleyear,"collect_date", c("month", "day", "year"), sep = "/"))
sample_year<-subset(sampleyear1, select = c("sta_seq","year"))

sample_year_distinct<-distinct(sample_year)
sampleyear.freq<-table(sample_year_distinct)
print(sampleyear.freq)


sampleyear.df<-as.data.frame.matrix(sampleyear.freq)

sampleyear.df<-cbind(sta_seq = row.names(sampleyear.freq), as.data.frame.matrix(sampleyear.freq))



sampleyear.df$years.frequency<-rowSums(sampleyear.df[2:6]=="1")

more.than.one.sites<- sampleyear.df %>%
                          filter(years.frequency > 1)

----------------------------------
##How many sites were collected in each major basin?

mbasn_basin<-subset(chem_basin,select = c("sta_seq","major"))

basin.unique<-unique(mbasn_basin$major)

Basin<-data.frame(Major.Basin=character(),num.sites=integer())

for (i in 1:length(basin.unique)) {
  major.basin<-basin.unique[i]
  stationcount<-dim(mbasn_basin[mbasn_basin$major==major.basin&mbasn_basin$duplicate==0,])[1]
  MajorBasin<-data.frame(major.basin,stationcount)
  MajorBasin.bind<-rbind(Basin,MajorBasin)
}
--------------------------------------
mbasn_basin<-subset(chem_basin,select = c("sta_seq","major"))
table.major.basin<-table(mbasn_basin)
table.major.df<-cbind(sta_seq = row.names(table.major.basin), as.data.frame.matrix(table.major.basin))
num.sites.per.mbasin<-colSums(table.major.basin[,-1])
major_basin_sitescount<-data.frame(num.sites.per.mbasin)

-----------------------------------------------

## What percentage of subregional basins in the State have one or more samples?
library(dplyr)

unique(chem_basin$sbasn)

samples.sbasn <- subset(chem_basin, select = c("sbasn","sta_seq"))

samples.sbasn.table<-table(samples.sbasn)

sbasn.df<-cbind(sbasn = row.names(samples.sbasn.table), as.data.frame.matrix(samples.sbasn.table))
sbasn.total<-data.frame(cbind(sbasn.df, total = rowSums(sbasn.df[-1])))

sbasn.sample.frequency<-subset(sbasn.total,select = c("sbasn", "total"))

num.sbasn.over1<-count(sbasn.sample.frequency, total > 1)
"percentage of sbasn with more than one sample"<-print(100 * (num.sbasn.over1$n / length(unique(sbasn.df$sbasn))))
-------------------------------------------------------

  ##What are the summary statistics for each parameter?

summary(Magnesium$value)
  
  


summary.parameter<-data.frame(chemparameter = character(), 
                              Min = integer(), 
                              first.Qu = integer(), 
                              Median = integer(), 
                              Mean = integer(), 
                              third.Qu = integer(),
                              Max. = integer (),
                              stringsAsFactors = FALSE)

chemunique<-(unique(chem_basin$chemparameter))

for (i in 1:length('chemunique')) {
  
  chem.i<-chemunique[i]
  
  parametersumm<-summary(chem_basin[chem_basin$chemparameter==chem.i&chem_basin$duplicate==0,])[1]
  
  summary.parameter1<-data.frame(chem.i,parametersumm)
  
  param.summ<-rbind(summary.parameter,summary.parameter1)
}


library(dplyr)
library(tidyr)

tbl.chembasin<-tbl_df(chem_basin)

for (i in 1:length(chemunique)) {
  chem.i<-chemunique[i]
 
  parametersumm <- tbl.chembasin %>%
    select_all(chemparameter) %>% # select variables to summarise
    summarise_all(funs(min = min, 
                      q25 = quantile(., 0.25), 
                      median = median, 
                      q75 = quantile(., 0.75), 
                      max = max,
                      mean = mean, 
                      sd = sd)) 
}

library(tidyverse)
chem_basin %>% 
  select(chemparameter) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "parameter")


-------------------------
parameter_summary <- function(x){
  c(mean = mean(x), sd = sd(x), min = min(x),max = max(x))
}

as.data.frame(lapply(list_parameters, sapply, parameter_summary))

--------------------------


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

list_parameters1<-as_tibble(list_parameters)
parametersumm <- Param_summ %>%
  select_all(Param_summ) 
  summarise_all(funs(min = min, 
                     q25 = quantile(., 0.25), 
                     median = median, 
                     q75 = quantile(., 0.75), 
                     max = max,
                     mean = mean, 
                     sd = sd)) 
Magnesium.stat<-as.data.frame.matrix(summary(list_parameters[[1]]))



library(data.table)

for (i in seq_along(list_parameters)) list_parameters[[i]]$Magnesium <- names(list_parameters)[i]
require(data.table)

DT.parameters <- rbindlist(list_parameters)

DT.parameters <-subset(DT.parameters, select = c("chemparameter","value"))


Param_summ<-data.frame(chemparameter=character(),value =integer(),stringsAsFactors=FALSE)


for (i in 1:length(chemunique)) {
  chem.i<-chemunique[i]
  
  parametersumm<-summary(DT.parameters[DT.parameters$chemparameter==chem.i&DT.parameters$duplicate==0,])
  
  Param_summ1<-data.frame(chem.i,parametersumm)
  
  Param_summ2<-rbind(Param_summ,Param_summ1)
  
  print(Param_summ2)
}


summary(Magnesium$value)



library(dplyr)
library(tidyr)
library(magrittr)
library(qwraps2)
library(rlang)

library(data.table)







for (i in 1:length(chemunique)) {
  chem.i<-chemunique[i]
  Param_summ<-summary(DT.parameters[DT.parameters$chemparameter==chem.i&DT.parameters$duplicate==0,])
}




filter(DT.parameters,chemparameter=="Magnesium")

install.packages("pastecs")




chemunique
col<-match(DT.parameters$chemparameter, chemunique)
row<-cumsum(c(0,diff(col))<=0)

Param_summ <- matrix(nrow=max(row), ncol=max(col))
colnames(Param_summ) <- chemunique
Param_summ[cbind(row, col)] <- DT.parameters$value



library(pastecs)
stat.desc(Param_summ)
options(scipen = 100)
options(digits = 2)
options(nbr.na = FALSE)
stat.desc(Param_summ)

library(purrr)
library(tidyverse)

summary.parameter<-split(DT.parameters,DT.parameters$chemparameter)

my_summary <- function(x) { 
  tibble(
    mean  = mean(x),
    median = median(x),
    min   = min(x), 
    max = max(x),
    anyNA = anyNA(x)
  )}

Param_summ2<-as.data.frame(lapply(summary.parameter, sapply, my_summary))
Param_summ<- select(Param_summ2, -contains("parameter"))
----

DT.parameters1 <-subset(chem_basin, select = c("chemparameter","value"))
summary.parameter1<-split(DT.parameters1,DT.parameters1$chemparameter)

for (i in 1:length(chemunique)) {
  parami<-chemunique[i]

lapply(seq_along(summary.parameter1), 
       function(i,x) {assign(paste0(chemunique[i]),x[[i]], envir=.GlobalEnv)},
       x=summary.parameter1)
}
----
  
paramDF <- data.frame(cbind(Magnesium, 
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
                            `Nitrate/Nitrite`))

data_long <- gather(paramDF, factor_key=TRUE)

data_long%>% group_by(key)%>%
  summarise(mean= mean(value), median= median(value), max = max(value),min = min(value))

------

my_summary <- function(x){
  c(mean = mean(x),
    median = median(x),
    min   = min(x), 
    max = max(x),
    anyNA = anyNA(x))
}
chempara<-as.data.frame(lapply(split.parameter1, sapply, my_summary))


#What are the summary statistics for each parameter in each major basin?


test<-aggregate(Ammonia["value"],by=list(chem_basin=Ammonia$major),FUN = summary)  
test  

tapply(chem_basin$value, chem_basin$major, function(x) format(summary(x), scientific = TRUE))  
  

uniquemajor<-unique(chem_basin$major)

for (i in 1:length(uniquechem)) {
  param<-chem_basin[chem_basin$chemparameter==uniquechem[i]&chem_basin$duplicate==0,4]
  
  cntNA<-length(param[is.na(param)])
  test<-aggregate(uniquechem[i]["value"],by=list(chem_basin=Ammonia$major),FUN = summary)  
}



library(dplyr)
library(qwraps2)
library(tidyverse)

str(chem_basin)


uniquemajor<-unique(chem_basin$major)

for (i in 1:length(uniquechem)) {
  param1<-uniquechem[i]
  our_summary <-
    list("param1" =
           list("min" = ~ min(.data$value),
                "max" = ~ max(.data$value),
                "mean (sd)" = ~ qwraps2::mean_sd(.data$value)),
                "median" = ~ median(.data$value))
  
}
summary <- summary.data.frame(dplyr::group_by(mbasin, major), our_summary)
summary



mbasin <- subset(chem_basin, select = c("chemparameter","value","major"))

split.mbasin <- split(mbasin,mbasin$major)

for (i in seq_along(split.mbasin)){
  split.mbasin[[i]]$dfname <- names(split.mbasin)[i]
  require(data.table)
  DT <- rbindlist(split.mbasin, fill = TRUE)
  DT[,lapply(.SD, mean), by =dfname]
}



