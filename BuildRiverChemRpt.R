library('RSQLite')
library(ggplot2)
library(scales)
library(ggthemes)
library(sf)
library(tmap)

setwd("/home/mkozlak/Documents/Projects/GitHub/5YrMonitoringRpt")

##########LOAD and format the data####################################################################
## Load data from the SQLite database#################################################################
db_path <- paste0(getwd(),'/data/')
db <- dbConnect(SQLite(), dbname=paste(db_path,"monrpt.db",sep=''));
SQL<- "SELECT chemdata.sta_seq, chemdata.collect_date, chemdata.chemparameter, chemdata.value, 
      chemdata.uom, chemdata.station_type, chemdata.duplicate, sites.name,  sites.ylat, sites.xlong, 
      sites.sbasn, basin.major, basin.mbasn, mdl.MDL
      FROM chemdata 
      JOIN sites ON chemdata.sta_seq = sites.sta_seq
      JOIN basin ON sites.sbasn = basin.sbasn
      JOIN mdl ON chemdata.chemparameter = mdl.chemparameter
      WHERE chemdata.station_type='River/Stream' AND chemdata.duplicate='0';"  
chem_basin<-dbGetQuery(conn=db,SQL);
dbDisconnect(db);

#Replace all NA values with MDL
chem_basin$value<- ifelse(is.na(chem_basin$value),chem_basin$MDL,chem_basin$value)

#######Function that creates cumulative frequency distribution plots for any parameter###################
#########################################################################################################
plotCFD<- function (data,chemicalParameter,plotSite){
            p<- (data[data$chemparameter==chemicalParameter,])
            pAvg<-aggregate(value~sta_seq+name+major+mbasn,data=p,FUN=mean)
            pSite<-pAvg[pAvg$sta_seq==plotSite,]
            pSiteRow<-as.numeric(rownames(pAvg)[pAvg$sta_seq==pSite$sta_seq])
            pBasin<- pAvg[pAvg$mbasn==pSite$mbasn,]
            axis_title <- element_text(face = "bold", color = "black")
            getUOM<-as.character(unique(p$uom))
            
            plot<-  ggplot(pAvg,aes(value)) +
                      labs(title = paste(pSite$name,"SID",pSite$sta_seq),x=paste0(chemicalParameter,"(",getUOM,")"), y="Cumulative percent of data") +
                      theme_bw()+
                      theme(text=element_text(size=16,  family="serif"))+
                      theme(title = axis_title, axis.title = axis_title)+
                      theme(plot.title = element_text(hjust = 0.5))+
                      scale_y_continuous(labels = percent)+
                      stat_ecdf(geom = "line", size = 1.1, colour = "#0000CC") + 
                      #stat_ecdf(data = pBasin, mapping = aes(value),geom = "line", linetype = "solid", size = 1.1, color = "#666666") + 
                      geom_point(aes(x=value[pSiteRow],y=ecdf(value)(value[pSiteRow])), size = 3.5, color = "red")+
                      geom_vline(xintercept = median(pAvg$value), size = 1.5)+
                      geom_vline(xintercept = median(pBasin$value), size=1.5, linetype="dotted",color = "red")+
                      
            if(chemicalParameter == "Total Phosphorus" | chemicalParameter == "Total Nitrogen"){
              plot<- plot + scale_x_log10()
            } else if (chemicalParameter == "Chloride"){
              plot<- plot + scale_x_sqrt()
            }
            
          return(plot)
}

#######Create a function to create a map for the report############################
###################################################################################

makeSiteMap<- function(data,plotSite){
                #Process data for map
                sites<-unique(data[c("sta_seq","name","ylat","xlong")])
                sitesSP<-st_as_sf(sites,coords=c('xlong','ylat'),crs=4326)#Create a spatial dataframe
                majBasinSP<-st_read("mapdata/majorbasin.geojson") #Read in static spatial layers
                riversSP<-st_read("mapdata/rivers.geojson")
  
                #Create your tmap here
                
}

#When you are finished this should create a map for a site
makeSiteMap(chem_basin,14314)



######EXAMPLES OF HOW TO Run the function for a given parameter and site################
######and example of how to use in a loop ##############################################
params<-c("Total Phosphorus","Total Nitrogen","Chloride","Total Suspended Solids")

plotCFD(chem_basin,params[2],"16124") #Example run for a particular parameter and site

###Example that creates a CFD plot for each param at each site###
###This produces 2432 plots for this dataset (i.e. 608 sites * 4 params)
SitePlots <- list()
SiteID <- unique(chem_basin$sta_seq)
#pdf("plots.pdf")

for (i in 1:length(params)){
  for (j in 1:length(SiteID)){
    p<-plotCFD(chem_basin,params[i],SiteID[j])
    SitePlots[[length(SitePlots)+1]]<-p
  }
}

#print(SitePlots)
#dev.off()

###function for cumulative frequency plots per major basin###EDIT###
# plotMajor<- function (chemicalParameter){
#    x<-(chem_basin[chem_basin$chemparameter==chemicalParameter,])
#    pMaj<-aggregate(chemparameter~major+value,data=x, FUN = mean, na.rm=TRUE)
#    axis_title <- element_text(face = "bold", color = "black")
#    getUOM<-as.character(unique(x$uom))
#    getParam<-as.character(unique(x$chemparameter))
#    
#    multi <-ggplot(data = pMaj, aes(x = value, group = major, col = major)) +
#       labs(title = paste0(getParam, "(",getUOM,")"), 
#            x = paste0(chemicalParameter,"(",getUOM,")"), 
#            y = "Cumulative percent of data\n")+
#       stat_ecdf(geom = "line", size = 1)+
#       scale_y_continuous(labels = percent)+
#       scale_x_continuous(breaks = seq(0,600, by=50))+
#       geom_vline(xintercept = median(pMaj$value), size = 1.1)+
#       scale_color_brewer(palette = "Set1")+
#       theme_stata()+
#       theme(title = axis_title, plot.title = element_text(size = 20, hjust = 0.5))+
#       theme(axis.title.x = element_text(size = 12, vjust = -0.5))+
#       theme(axis.title.y = element_text(size = 12))+
#       theme(legend.title=element_blank())
#    
#    return(multi)
# }
# 
# 
# #majors ("Housatonic","Connecticut","Southwest Coast","South Central Coast","Pawcatuck","Southeast Coast","Thames","Hudson")
# 
# plotMajor("Chloride")







