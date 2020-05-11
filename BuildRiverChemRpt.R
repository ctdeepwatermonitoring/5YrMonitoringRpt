library('RSQLite')
library(ggplot2)
library(scales)

setwd("")

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
plotCFD<- function (chemicalParameter,plotSite){
            p<- (chem_basin[chem_basin$chemparameter==chemicalParameter,])
            pAvg<-aggregate(value~sta_seq+name+major+mbasn,data=p,FUN=mean)
            pSite<-pAvg[pAvg$sta_seq==plotSite,]
            pSiteRow<-as.numeric(rownames(pAvg)[pAvg$sta_seq==pSite$sta_seq])
            pBasin<- pAvg[pAvg$mbasn==pSite$mbasn,]
            axis_title <- element_text(face = "bold", color = "black")
            getUOM<-as.character(unique(p$uom))
            
            plot<-  ggplot(pAvg,aes(value)) +
                      labs(title = paste(pSite$name,"SID",pSite$sta_seq),x=paste0(chemicalParameter,"(",getUOM,")"), y="Cumulative percent of data") +
                      theme_light()+
                      theme(title = axis_title, axis.title = axis_title)+
                      theme(plot.title = element_text(hjust = 0.5))+
                      scale_y_continuous(labels = percent)+
                      stat_ecdf(geom = "line", size = 1.1, colour = "#00AFBB") + 
                      stat_ecdf(data = pBasin, mapping = aes(value),geom = "line", size = 1.1, colour = "#40004b") + 
                      geom_point(aes(x=value[pSiteRow],y=ecdf(value)(value[pSiteRow])))+
                      geom_vline(xintercept = median(pAvg$value), size = 1.5)
                      #geom_vline(xintercept = mean(param$value), size=1.5, linetype="dotted",color = "red")+
                      
            if(chemicalParameter == "Total Phosphorus" | chemicalParameter == "Total Nitrogen"){
              plot<- plot + scale_x_log10()
            } else if (chemicalParameter == "Chloride"){
              plot<- plot + scale_x_sqrt()
            }
            
          return(plot)
}


######Run the function for a given parameter and site###################################
########################################################################################
params<-c("Total Phosphorus","Total Nitrogen","Chloride","Total Suspended Solids")

plotCFD(params[4],"16124") #Example run for a particular parameter and site