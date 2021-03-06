library('RSQLite')
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(tidyr)

setwd("C:/Users/kevin/Documents/Projects/GitHub/DEEP QA/5YrMonitoringRpt")
env<-read.csv("data/site_env.csv",header=TRUE)
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


#vector of unique chemical parameters
uniqueChem<-unique(chem_basin$chemparameter)


##what chemical parameters were collected?
  
uniqueChem<-unique(chem_basin$chemparameter)
ChemParam<-data.frame(uniquechem)

##How many river and stream sites were samples from 2011 through 2015? (edit)
  
NumberSites<-length(chem_basin$collect_date)
Number_of_sites<-data.frame(NumberSites)

##How many samples were collected for each chemical parameter at river/stream sites? 
  
  #create vector of unique chem parameters
chemunique<-unique(chem_basin$chemparameter)

  #create empty dataframe to store chem parameter and count of samples
ChemSamp<-data.frame(chemparameter=character(),numsample=integer(),stringsAsFactors=FALSE)

  #loop for unique chem parameters
for (i in 1:length(chemunique)){
  #identify parameter(s)
  parameter.i<-chemunique[i]
  #count the # of samples 
  samplecount<-dim(chem_basin[chem_basin$chemparameter==parameter.i&chem_basin$duplicate==0,])[1]
  #create df w/ parameter(s) and count of sample 
  ChemSampParam<-data.frame(parameter.i,samplecount)
  #append df to empty df created outside of loop
  ChemSamp<-rbind(ChemSamp,ChemSampParam)
  
}
  #create df*
Parameter_Count<-data.frame(ChemSamp)


##Do all of the samples have the same unit of measure for each given parameter?
  
  #subset table 
uom_chemparameter<-chem_basin[,c("chemparameter","uom")]

  #extract unique rows from table
unique_uom <- 
  data.frame(uom_chemparameter %>% distinct(chemparameter,uom,.keep_all = TRUE))

  #find common values (dplyr)
inner_join(uom_chemparameter, unique_uom)

  #identify rows in uom_chemparameter that are not present in unique_uom (dplyr)*
error_uom<-data.frame(setdiff(uom_chemparameter, unique_uom))


##How many river and stream samples were collected excluding duplicates?
  #exclude duplicates
nodup_chem<-distinct(chem_basin,.keep_all=TRUE)
Num_of_sample<-data.frame(length(nodup_chem$station_type))

##How many river and stream samples (excluding duplicates) were collected in each year? (edit)

#create df for dates

dates <- data.frame(chem_basin[2], stringsAsFactors = FALSE)

#separate dates into "month", "day", "year"
month_day_year<-data.frame(separate(dates,"collect_date", c("month", "day", "year"), sep = "/"))

#create freq. table for sample years
sample_year_freq<-as.data.frame(table(data.frame(month_day_year[3])))
  names(sample_year_freq)[names(sample_year_freq) == 'Var1'] <- 'Year'
  
##How many sites were collected in more than one year? ##What sites were collected in more than one year and how many years were collected?
  
  #extract "year" from collection date
sampleYear<-
  data.frame(separate((chem_basin[1:2]),"collect_date", c("month", "day", "year"), sep = "/"))
  
  #subset df for station and year, remove duplicates
sample_year_distinct<-
  distinct(subset(sampleYear, select = c("sta_seq","year")))
  
  #create frequency table - dataframe, sta_seq as a new column
sample_year_freq<-
  cbind(sta_seq = row.names(sample_year_freq), 
          as.data.frame.matrix(table(sample_year_distinct)))
  
  #add col w/ stations visited in more than one year
sample_year_freq$years.frequency<-rowSums(sample_year_freq[2:6]=="1")
  
  #filter out sites collected in more than one year
more_than_one<- sample_year_freq %>%
  filter(years.frequency > 1)
  
  #number of sites sampled in more than one year*
length(more_than_one$sta_seq)

##How many sites were collected in each major basin?
  #subset chem_basin
table_major_basin<-
  (table(subset(chem_basin,select = c("sta_seq","major"))))

  #sta_seq as new column
table_major_basin<-cbind(sta_seq = row.names(table_major_basin), 
                         as.data.frame.matrix(table_major_basin))

  #sum, exclude sta_seq
sites_per_mbasin<-data.frame(colSums(table_major_basin[,-1]))
colnames(sites_per_mbasin)[1] <- "Site Frequency"
   
## What percentage of subregional basins in the State have one or more samples? (edit)

  #create frequency table
sbasn<-table(subset(chem_basin, select = c("sbasn", "sta_seq")))
  #sbasn as column 1, convert to matrix df
sbasn_total<-cbind(sbasn = row.names(sbasn), as.data.frame.matrix(sbasn))
  #add sum of row as last column
sbasn_total<-data.frame(cbind(sbasn_total, total = rowSums(sbasn_total[-1])))
  sbasn_sample_frequency<-count(sbasn_total, total > 1)
  
"percentage of subregional basins with more than one sample"<-print(100 * (sbasn.sample.frequency$n / length(unique(sbasn.total$sbasn))))     


##What are the summary statistics for each parameter? ##Only calculate summary statistics for river/stream samples and non-duplicates
  #Create a summary stats dataframe
summary_Stats<-data.frame(param=character(),Min=numeric(),Q1 =numeric(),Median=numeric(),
                          Mean=numeric(),Q3 =numeric(),
                          Max=numeric(),NAs=integer())

for (i in 1:length(uniqueChem)){
  param<-chem_basin[chem_basin$chemparameter==uniquechem[i]&chem_basin$duplicate==0,4]
  cntNA<-length(param[is.na(param)])
  test<-(summary(param)[1:6])
  test<-as.data.frame(as.matrix(test))
  test<-data.frame(param=uniqueChem[i],Min=test[1,],Q1=test[2,],Median=test[3,],
                   Mean=test[4,],Q3=test[5,],Max=test[6,],NAs=cntNA)
  summary_Stats<-rbind(test,summary_Stats)
}  


##What are the summary statistics for each parameter in each major basin? 
  #aggregate values based on major basin & parameter
mbasin_para<-aggregate(chem_basin$value , by = list(chem_basin$major , chem_basin$chemparameter )  , FUN = summary)  


#rename columns
mbasin_para<- mbasin_para %>% 
  dplyr::rename(
    MajorBasin = Group.1,
    Chemparameter = Group.2,
    SummaryStats = x
  )
#sort columns by major basin
mbasin_para<-data.frame(arrange(mbasin_para,MajorBasin))


##summary stats 
summStat =  c(min = "min", 
              q1 = ~quantile(.,probs = 0.25), 
              median = "median", 
              q2 = ~quantile(.,probs = 0.75), 
              max = "max",
              mean = "mean", 
              sd = "sd")


##what are the summary stats for drainage area for the sites? By major basin?
  
StationMajor<-
  data.frame(subset(distinct(chem_basin,sta_seq, .keep_all = TRUE), 
                    select = c("sta_seq","major")))

  #ID mismatch*
anti_join(env,StationMajor)

Majorenv<-
  subset(merge(StationMajor[StationMajor$sta_seq!=14302,],env,by="sta_seq"), 
         select = c("major","SqMi"))

  #summary stats drainage area (miles)
summary(Majorenv$SqMi)

  #summary stats drainage area (miles) by major basin
Drainagearea<-as.data.frame(Majorenv %>% 
                              group_by(major) %>%
                              summarise_at(.vars = names(.)[2],.funs = summStat))


##What are the summary stats for percent impervious cover for the sites? By major basin?

  #summary stats percent impervious cover
summary(Majorenv$IC_Avg)

Majorenv_IC<-subset(merge(StationMajor[StationMajor$sta_seq!=14302,],env,by="sta_seq"), 
                    select = c("major","IC_Avg"))

  #summary stats percent impervious cover by major basin
PercentIC<-as.data.frame(Majorenv_IC %>% 
                           group_by(major) %>%
                           summarise_at(.vars = names(.)[2],.funs = summStat))

##Table Major Basin - Chem Parameter

  #summary stats graphics
boxStat = c(count = "length",
            min = "min",
            q1 = ~quantile(.,probs = 0.25, na.rm = TRUE), 
            median = "median", 
            q2 = ~quantile(.,probs = 0.75, na.rm = TRUE), 
            max = "max",
            mean = "mean", 
            sd = "sd",
            SE = "SE")

  #standard error function
SE <- function(x) sd(x)/sqrt(length(x))

  #Major Basin order
MajorLevel = c("Connecticut",
               "Thames",
               "Southwest Coast",
               "Southeast Coast",
               "South Central Coast",
               "Pawcatuck",
               "Hudson",
               "Housatonic",
               "Statewide")

mbasin_para1 <- na.omit(subset(chem_basin, select = c("major","chemparameter","value")))
mbasin_para1$chemparameter <- gsub(" ", ".", mbasin_para1$chemparameter)
mbasin_para1<-as.data.frame(mbasin_para1 %>% 
                              group_by(major,chemparameter) %>%
                              summarise_at(.vars = names(.)[3],.funs = boxStat))

mbasin_para1 <- mbasin_para1 %>% 
  mutate_if(is.numeric, round, digits = 2)

  #Statewide summary stats
omitna_chembasin <- na.omit(chem_basin)
Statewide <- as.data.frame(omitna_chembasin %>%
                             group_by(chemparameter) %>%
                             summarise_at(.vars = names(.)[4],.funs = boxStat))
Statewide<- t(Statewide)
colnames(Statewide) <- Statewide[1,]
Statewide <- (Statewide[-1,])

  
##Chloride Graphics
chloride <- mbasin_para1[mbasin_para1$chemparameter == "Chloride", ]  
chloride<- t(chloride)
colnames(chloride) <- chloride[1, ]
chloride <- chloride[-1, ]
chloride <- chloride[-1, ]

chloride_state <- subset(Statewide, select = "Chloride")
chloride_state <- as.numeric(as.character(chloride_state))

chloride <-data.frame(cbind(chloride, chloride_state))

names(chloride)[names(chloride) == 'chloride_state'] <- 'Statewide'

  #boxplot Chloride

chloride2 <- data.frame(na.omit(subset(chem_basin, select = c("chemparameter","value"))))
chloride2 <- data.frame(subset(chloride2, chloride2$chemparameter == "Chloride"))


chloride2['major'] = 'Statewide'


chloride1 <- data.frame(na.omit(subset(chem_basin, select = c("major","chemparameter","value"))))
chloride1 <- data.frame(subset(chloride1, chloride1$chemparameter == "Chloride"))
chloride1 <- arrange(chloride1, chloride1$major)
chloride1 <- data.frame(rbind(chloride1, chloride2))


chloride1$major <- as.factor(chloride1$major)
head(chloride1)
chloride1$major <- factor(chloride1$major , levels = MajorLevel)


chlorideBoxplot <- ggplot(chloride1, aes(x= major, y= value, fill = major)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=8,
               outlier.size=2)+
  labs(title = "Chloride",x="Major Basin", y="ppm")+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_summary(fun = mean, geom="point", shape=23, size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  theme(legend.position="none")

chlorideBoxplot

##Total Phosphorus Graphics

  #Total Phosphorus summary stat by Major Basin 
TotalPhosphorus <- mbasin_para1[mbasin_para1$chemparameter == "Total.Phosphorus",]  

TotalPhosphorus<- t(TotalPhosphorus)
colnames(TotalPhosphorus) <- TotalPhosphorus[1, ]
TotalPhosphorus <- TotalPhosphorus[-1, ]
TotalPhosphorus <- TotalPhosphorus[-1, ]

Phosphorus_state <- subset(Statewide, select = "Total Phosphorus")
Phosphorus_state <- as.numeric(as.character(Phosphorus_state))

TotalPhosphorus <-data.frame(cbind(TotalPhosphorus, Phosphorus_state))

names(TotalPhosphorus)[names(TotalPhosphorus) == 'Phosphorus_state'] <- 'Statewide'


  #boxplot Total Phosphorus

TotalPhosphorus1 <- data.frame(na.omit(subset(chem_basin, select = c("chemparameter","value"))))
TotalPhosphorus1 <- data.frame(subset(TotalPhosphorus1, TotalPhosphorus1$chemparameter == "Total Phosphorus"))


TotalPhosphorus1['major'] = 'Statewide'


TotalPhosphorus2 <- data.frame(na.omit(subset(chem_basin, select = c("major","chemparameter","value"))))
TotalPhosphorus2 <- data.frame(subset(TotalPhosphorus2, TotalPhosphorus2$chemparameter == "Total Phosphorus"))
TotalPhosphorus2 <- arrange(TotalPhosphorus2, TotalPhosphorus2$major)
TotalPhosphorus2 <- data.frame(rbind(TotalPhosphorus2, TotalPhosphorus1))
TotalPhosphorus2$major <- as.factor(TotalPhosphorus2$major)
head(TotalPhosphorus2)
TotalPhosphorus2$major <- factor(TotalPhosphorus2$major , levels = MajorLevel)


Phosphorus_box <- ggplot(TotalPhosphorus2, aes(x= major, y= value, fill = major)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=8,
               outlier.size=2)+
  labs(title = "Total Phosphorus",x="Major Basin", y="ppm")+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_summary(fun = mean, geom="point", shape=23, size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  theme(legend.position="none")

Phosphorus_box

##Total Nitrogen Graphics

#Total Nitrogen summary stat by Major Basin 
TotalNitrogen <- mbasin_para1[mbasin_para1$chemparameter == "Total.Nitrogen",]  

TotalNitrogen<- t(TotalNitrogen)
colnames(TotalNitrogen) <- TotalNitrogen[1, ]
TotalNitrogen <- TotalNitrogen[-1, ]
TotalNitrogen <- TotalNitrogen[-1, ]

Nitrogen_state <- subset(Statewide, select = "Total Nitrogen")
Nitrogen_state <- as.numeric(as.character(Nitrogen_state))

TotalNitrogen <-data.frame(cbind(TotalNitrogen, Nitrogen_state))

names(TotalNitrogen)[names(TotalNitrogen) == 'Nitrogen_state'] <- 'Statewide'


#boxplot Total Nitrogen

TotalNitrogen1 <- data.frame(na.omit(subset(chem_basin, select = c("chemparameter","value"))))
TotalNitrogen1 <- data.frame(subset(TotalNitrogen1, TotalNitrogen1$chemparameter == "Total Nitrogen"))


TotalNitrogen1['major'] = 'Statewide'


TotalNitrogen2 <- data.frame(na.omit(subset(chem_basin, select = c("major","chemparameter","value"))))
TotalNitrogen2 <- data.frame(subset(TotalNitrogen2, TotalNitrogen2$chemparameter == "Total Nitrogen"))
TotalNitrogen2 <- arrange(TotalNitrogen2, TotalNitrogen2$major)
TotalNitrogen2 <- data.frame(rbind(TotalNitrogen2, TotalNitrogen1))
TotalNitrogen2$major <- as.factor(TotalNitrogen2$major)
head(TotalNitrogen2)
TotalNitrogen2$major <- factor(TotalNitrogen2$major , levels = MajorLevel)


Nitrogen_box <- ggplot(TotalNitrogen2, aes(x= major, y= value, fill = major)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=8,
               outlier.size=2)+
  labs(title = "Total Nitrogen",x="Major Basin", y="ppm")+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_summary(fun = mean, geom="point", shape=23, size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  theme(legend.position="none")

Nitrogen_box


##Hardness Graphics (change y scale?)

  #Hardness summary stat by Major Basin 
Hardness <- mbasin_para1[mbasin_para1$chemparameter == "Hardness",]  

Hardness<- t(Hardness)
colnames(Hardness) <- Hardness[1, ]
Hardness <- Hardness[-1, ]
Hardness <- Hardness[-1, ]

Hardness_state <- subset(Statewide, select = "Hardness")
Hardness_state <- as.numeric(as.character(Hardness_state))

Hardness <-data.frame(cbind(Hardness, Hardness_state))

names(Hardness)[names(Hardness) == 'Hardness_state'] <- 'Statewide'


  #boxplot Hardness

Hardness1 <- data.frame(na.omit(subset(chem_basin, select = c("chemparameter","value"))))
Hardness1 <- data.frame(subset(Hardness1, Hardness1$chemparameter == "Hardness"))


Hardness1['major'] = 'Statewide'


Hardness2 <- data.frame(na.omit(subset(chem_basin, select = c("major","chemparameter","value"))))
Hardness2 <- data.frame(subset(Hardness2, Hardness2$chemparameter == "Hardness"))
Hardness2 <- arrange(Hardness2, Hardness2$major)
Hardness2 <- data.frame(rbind(Hardness2, Hardness1))
Hardness2$major <- as.factor(Hardness2$major)
head(Hardness2)
Hardness2$major <- factor(Hardness2$major , levels = MajorLevel)


Hardness_box <- ggplot(Hardness2, aes(x= major, y= value, fill = major)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=8,
               outlier.size=2)+
  labs(title = "Hardness",x="Major Basin", y="ppm")+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_summary(fun = mean, geom="point", shape=23, size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  theme(legend.position="none")

Hardness_box

##Turbidity Graphics

  #Turbidity summary stat by Major Basin 
Turbidity <- mbasin_para1[mbasin_para1$chemparameter == "Turbidity",]  

Turbidity<- t(Turbidity)
colnames(Turbidity) <- Turbidity[1, ]
Turbidity <- Turbidity[-1, ]
Turbidity <- Turbidity[-1, ]

Turbidity_state <- subset(Statewide, select = "Turbidity")
Turbidity_state <- as.numeric(as.character(Turbidity_state))

Turbidity <-data.frame(cbind(Turbidity, Turbidity_state))

names(Turbidity)[names(Turbidity) == 'Turbidity_state'] <- 'Statewide'


  #boxplot Turbidity

Turbidity1 <- data.frame(na.omit(subset(chem_basin, select = c("chemparameter","value"))))
Turbidity1 <- data.frame(subset(Turbidity1, Turbidity1$chemparameter == "Turbidity"))


Turbidity1['major'] = 'Statewide'


Turbidity2 <- data.frame(na.omit(subset(chem_basin, select = c("major","chemparameter","value"))))
Turbidity2 <- data.frame(subset(Turbidity2, Turbidity2$chemparameter == "Turbidity"))
Turbidity2 <- arrange(Turbidity2, Turbidity2$major)
Turbidity2 <- data.frame(rbind(Turbidity2, Turbidity1))
Turbidity2$major <- as.factor(Turbidity2$major)
head(Turbidity2)
Turbidity2$major <- factor(Turbidity2$major , levels = MajorLevel)


Turbidity_box <- ggplot(Turbidity2, aes(x= major, y= value, fill = major)) + 
  geom_boxplot(outlier.colour="black", outlier.shape=8,
               outlier.size=2)+
  labs(title = "Turbidity",x="Major Basin", y="NTU")+
  scale_fill_brewer(palette="RdBu")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_summary(fun = mean, geom="point", shape=23, size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  annotation_logticks(sides = "l")+
  theme(legend.position="none")

Turbidity_box

##axis title
axis_title <- element_text(face = "bold", color = "black")

##Cumulative Frequency Chloride Statewide (edit)
chlorideCf = chloride2$value 

  #set df
chlorideCf <- data.frame(chlorideCf)
chlorideCf1 <- as.numeric(unlist(chlorideCf))

Chloride_cfplot <- ggplot(chlorideCf, aes(chlorideCf)) +
  labs(title = "Chloride Statewide ",x="Chloride (ppm)", y="Cumulative percent of data") +
  theme_light()+
  theme(title = axis_title, axis.title = axis_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = percent)+
  stat_ecdf(geom = "line", size = 1.1, colour = "#00AFBB") + 
  scale_x_continuous(name="Chloride (ppm)", limits=c(0, 100))+
  geom_vline(xintercept = median(chlorideCf1), size = 1.5)+
  geom_vline(xintercept = mean(chlorideCf1), size=1.5, linetype="dotted",color = "red")


Chloride_cfplot
  
##Cumulative Frequency Phosphorus Statewide (edit)
phosphorusCf = TotalPhosphorus1$value

#axis title
axis_title <- element_text(face = "bold", color = "black")

#set df
phosphorusCf <- data.frame(phosphorusCf)
phosphorusCf1 <- as.numeric(unlist(phosphorusCf))

phosphorus_cfplot <- ggplot(phosphorusCf, aes(phosphorusCf)) +
  labs(title = "Total Phosphorus Statewide ",x="Total Phosphorus (ppm)", y="Cumulative percent of data") +
  theme_light()+
  theme(title = axis_title, axis.title = axis_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = percent)+
  stat_ecdf(geom = "line", size = 1.1, colour = "#00AFBB") + 
  scale_x_continuous(name="Total Phosphorus (ppm)", limits=c(0,1.5))+
  geom_vline(xintercept = median(phosphorusCf1), size = 1.5)+
  geom_vline(xintercept = mean(phosphorusCf1), size=1.5, linetype="dotted",color = "red")


phosphorus_cfplot  

##Cumulative Frequency Nitrogen Statewide (edit)
nitrogenCf = TotalNitrogen1$value

#set df
nitrogenCf <- data.frame(nitrogenCf)
nitrogenCf1 <- as.numeric(unlist(nitrogenCf))

nitrogen_cfplot <- ggplot(nitrogenCf, aes(nitrogenCf)) +
  labs(title = "Total Nitrogen Statewide ",x="Total Nitrogen (ppm)", y="Cumulative percent of data") +
  theme_light()+
  theme(title = axis_title, axis.title = axis_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = percent)+
  stat_ecdf(geom = "line", size = 1.1, colour = "#00AFBB") + 
  scale_x_continuous(name="Total Nitrogen (ppm)", limits=c(0,6.625))+
  geom_vline(xintercept = median(nitrogenCf1), size = 1.5)+
  geom_vline(xintercept = mean(nitrogenCf1), size=1.5, linetype="dotted",color = "red")


nitrogen_cfplot  

##Cumulative Frequency Hardness Statewide (edit)
hardnessCf = Hardness1$value

#set df
hardnessCf <- data.frame(hardnessCf)
hardnessCf1 <- as.numeric(unlist(hardnessCf))

hardness_cfplot <- ggplot(hardnessCf, aes(hardnessCf)) +
  labs(title = "Hardness Statewide ",x="Hardness (ppm)", y="Cumulative percent of data") +
  theme_light()+
  theme(title = axis_title, axis.title = axis_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = percent)+
  stat_ecdf(geom = "line", size = 1.1, colour = "#00AFBB") + 
  scale_x_continuous(name="Hardness (ppm)", limits=c(0,325))+
  geom_vline(xintercept = median(hardnessCf1), size = 1.5)+
  geom_vline(xintercept = mean(hardnessCf1), size=1.5, linetype="dotted",color = "red")


hardness_cfplot  

##Cumulative Frequency Turbidity Statewide (edit)
turbidityCf = Turbidity1$value

#set df
turbidityCf <- data.frame(turbidityCf)
turbidityCf1 <- as.numeric(unlist(turbidityCf))

turbidity_cfplot <- ggplot(turbidityCf, aes(turbidityCf)) +
  labs(title = "Turbidity Statewide ",x="Turbidity (NTU)", y="Cumulative percent of data") +
  theme_light()+
  theme(title = axis_title, axis.title = axis_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = percent)+
  stat_ecdf(geom = "line", size = 1.1, colour = "#00AFBB") + 
  scale_x_continuous(name="Turbidity (NTU)", limits=c(0,50))+
  geom_vline(xintercept = median(turbidityCf1), size = 1.5)+
  geom_vline(xintercept = mean(turbidityCf1), size=1.5, linetype="dotted",color = "red")


turbidity_cfplot  

##multi edcf Major Basin

library(ggthemes)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")

##Chloride multi edcf

Chloride_multi <-ggplot(data = chloride1, aes(x = value, group = major, col = major)) +
  stat_ecdf(geom = "line", size = 1)+
  scale_x_continuous(breaks = seq(0,600, by=50))+
  scale_y_continuous(labels = percent)+
  scale_color_brewer(palette = "Set1")+
  labs(title = "Chloride (ppm) ")+
  xlab("\nChloride (ppm)")+
  ylab("Cumulative percent of data\n")+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = median(chlorideCf1), size = 1, linetype = "solid")+
  guides(color = guide_legend(reverse = TRUE))+
  theme(legend.title=element_blank())
  

Chloride_multi




