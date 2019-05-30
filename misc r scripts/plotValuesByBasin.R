library('RSQLite')
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

dbDisconnect(db);

#Join in R syntax to create a table with sites and chemicals
table_basin<-merge(table,sitesbasin,by="sta_seq")

#decide chemChoice
chemChoice <- 'Hardness'

##all chloride values by major basin
chem <- table_basin[table_basin$chemparameter == chemChoice & table_basin$duplicate==0
                    & table_basin$value != 'NULL', ]

#check that correct amount of chemicals sync
#nrow(chem)

##aggregate chemical paramter'd values by major basin
#chem_appearances_by_basin <- aggregate(chem["value"], by=list(MajorBasin = chem$major), FUN=length)
#chem_appearances_by_basin

# the big leagues
#https://ggplot2.tidyverse.org/reference/geom_boxplot.html
#https://owi.usgs.gov/R/training-curriculum/intro-curriculum/ggplot2/
chem_plot <- ggplot(data=chem, aes(group = major, x = major, y = value)) +
#<- ggplot(data=chem, aes(group = mbasn, x = mbasn, y = value)) +    #alt: group by mbasn integer
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
    panel.grid.minor = element_blank()
  )

chem_plot


# display mbasn ids to major basin
#rbind(unique(chem$mbasn), unique(chem$major))

##scatterplot
# ggplot(data=chem, aes(x = mbasn, y = value)) +
#   labs(
#     title="Chloride",
#     x="Major Basin",
#     y="ppm"
#   ) +
#   geom_point()

