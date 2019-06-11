#The purpose of this script to to plot a bar graph counting milliQ values by count
#the title will be <chemparameter by year>
#if a value is less than the mdl, make it the mdl
#1 and 2 standard deviations above and below the mean will be displayed as wellppp;0

library(RSQLite)
library(ggplot2)

#open ODBC
db_path <- 'S:/J_Tonfa/5YrMonitoringRpt/' 
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));

#get table of Milli-Q values, identified by chemparameter and collection date
SQL <- "
select
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

dbDisconnect(db);

#--- LOOP CONCEPT ---#
#vector of years 2011 to 2015
v_years <- c(2011:2015)

#chosen chemical parameter
chemChoice <- 'Total Nitrogen'

#get table of just chemical parameters
chem <- table_milliQ[table_milliQ$chemparameter == chemChoice, ]

#convert null milliQ values to mdl
mdl <- unique(chem$MDL) #will return error if chem has more than one mdl
chem$Milli_Q[is.na(chem$Milli_Q)] <- mdl

#convert $collect_date to Date format
chem$collect_date <- as.Date(chem$collect_date, "%m/%d/%Y")

#calc mean and standard deviation
chem_mean <- mean(chem$Milli_Q)
chem_sd <- sd(chem$Milli_Q)

max_milli <- max(chem$Milli_Q)


#loop that goes from 2011 to 2015
for (year in v_years) {
  #manual set year
  #year <- 2013
  
  #create temp_chem dataframe of specific year
  temp_chem <- subset(chem, format.Date(chem$collect_date, "%Y")==year)
  
  title<-paste(toString(chemChoice), "by", toString(year))
  
  #graph chem_plot <- 
  chem_plot <- ggplot(data = temp_chem, aes(x = collect_date, y = Milli_Q)) +
  geom_bar(
      stat = "identity",
      position="dodge"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5) #center title
    ) + 
    labs(
      x = "Dates",
      y = "Milli-Q Value (starting at MDL)",
      title = title
    ) +
    #set y scale to start at mdl and end at max value
    coord_cartesian(ylim=c(mdl, max_milli)) +
    scale_y_continuous(breaks = c(mdl) + seq(0, max_milli, 0.05)) +
    #create legend and hlines
    geom_hline(
      aes(
        yintercept= chem_mean,
        linetype = paste("mean", round(chem_mean,digits=4))),
      colour= 'blue'
      ) +
    geom_hline(
      aes(
        yintercept= (chem_mean + chem_sd),
        linetype = paste("stDev", round(chem_sd,digits=4))),
      colour= 'red'
      ) +
    geom_hline(
        yintercept= chem_mean + (chem_sd*2), colour= 'purple', lty="dashed") +
    scale_linetype_manual(
      name = "limit",
      values = c(2, 2), 
      guide = guide_legend(
        override.aes = list(color = c("blue", "red"))
        )
      )
  
  #stop
  file_chem <- paste("S:/J_Tonfa/5YrMonitoringRpt/chemParameters/Results/milli-Q bars/", title, "-bar.png", sep="")
  ggsave(file_chem, plot = chem_plot, width = 20, height = 15, units = "cm")

}
#-----END-LOOP------#
