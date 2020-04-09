dates = dates %>%
  mutate(dates= ymd(dates)) %>% 
  mutate_at(vars(dates), list(month, day, year))


dates %>%
  dplyr::mutate(year = lubridate::year(dates), 
                month = lubridate::month(dates), 
                day = lubridate::day(dates))




dates <- data.frame(dates, stringsAsFactors = FALSE)
datesvec<-(chem$collect_date)


for (i in 1:length(dates)) {
  chemdate1 <- separate(dates[i], c("month", "day", "year"), sep = "/")
  length(unique("year"))
}


dates %>% 
  separate(dates, c("month", "day", "year"), sep = "/")
length(unique(dates$year))

chem1 %>%
  select(collect_date,sta_seq) %>%
  filter((is.na(collect_date))) %>%
  group_by(collect_date) %>%
  summarize(collect_date) %>%

filter(chem_basin, collect_date == "6/01/2011")

for (i in 1:length(uniqueyear)) {
  YearSamp<-uniqueyear[i]
  countyear<-data.frame(length(uniqueyear[1])
                     
  sampleyear.df<-data.frame(uniqueyear,countyear[i])
}

for (i in 1:length(uniqueyear)) {
  YearSamp<-uniqueyear[i]
  data.2011<-data.frame(sampleyear %>% filter_all(any_vars(. %in% c('2011'))))
  data.2012<-data.frame(sampleyear %>% filter_all(any_vars(. %in% c('2012'))))
  data.2013<-data.frame(sampleyear %>% filter_all(any_vars(. %in% c('2013'))))
  data.2014<-data.frame(sampleyear %>% filter_all(any_vars(. %in% c('2014'))))
  data.2015<-data.frame(sampleyear %>% filter_all(any_vars(. %in% c('2015'))))
  
  Samp_year <- data.frame(year=integer(), 
                         numsample=integer(), 
                         stringsAsFactors=FALSE)
  
  sampleyear.df<-data.frame(table(sampleyear))
  
}


  
  