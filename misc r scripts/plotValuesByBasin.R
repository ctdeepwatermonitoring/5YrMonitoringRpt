library('RSQLite')
library('ggplot2')
library('plyr')
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

#get mdl (minimum detection limit) table
SQL <- "SELECT *
FROM mdl;"

table_mdl <- dbGetQuery(conn=db, SQL)

dbDisconnect(db);

#Join in R syntax to create a table with sites and chemicals
table_basin<-merge(table,sitesbasin,by="sta_seq")

#decide chemChoice
chemChoice <- 'Turbidity'

##create data-frame of data
chem <- table_basin[table_basin$chemparameter == chemChoice & table_basin$duplicate==0, ]

#convert all NULL or N/A values to chemical's MDL (minimum detection limit)
test_table_mdl <- table_mdl[table_mdl$chemparameter == chemChoice &
                              table_mdl$MDL, ]
chem$value[is.na(chem$value)] <- test_table_mdl$MDL

#statewide total = create copy data frame with all values to major 'Total"
chem_total <- chem
chem_total$major <- "Total"

#combine two data frames
combined_chem <- rbind.fill(chem_total, chem) #.fill is plyr function

##the big leagues
#https://ggplot2.tidyverse.org/reference/geom_boxplot.html
#https://owi.usgs.gov/R/training-curriculum/intro-curriculum/ggplot2/
chem_plot <- ggplot(data=combined_chem, aes(group = major, x = major, y = value)) +
#<- ggplot(data=combined_chem, aes(group = mbasn, x = mbasn, y = value)) +    #alt: group by mbasn integer
  labs( # set labels
    title= chemChoice,
    x='Major Basin',
    y='ppm'
  ) +
  geom_boxplot(
    aes(color=major) #set colors for major basins
  ) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size=15), #change font size
    axis.text.x = element_text(size=16, face="bold", angle=45), #change x-axis labels
    plot.title = element_text(hjust = 0.5) #center title
  ) +
  #expand_limits(y=c(0, 7)) + #extend axis scale (and label)
  scale_y_continuous(breaks = seq(0, 1000, 5)) + #set tick interval to 50, limit 1000
  theme(legend.position="none") #remove legend

chem_plot

file_chem <- paste("S:/J_Tonfa/5YrMonitoringRpt/chemParameters/Results/", as.character(chemChoice), "-boxplot.png", sep="")
file_chem
ggsave(file_chem, plot = chem_plot, width = 15, height = 10, units = "in", dpi=72)
