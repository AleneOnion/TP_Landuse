# setwd("C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Lakes Database/Current")
# source("new_database/Reading.LMAS.Data.R")
# setwd("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse")
rm(list=setdiff(ls(), c("newdata","lake","status")))

habs <- status %>%
  mutate(LAKE_HISTORY_ID=ifelse(grepl("-",LOCATION_HISTORY_ID),LOCATION_HISTORY_ID,gsub("_.*","",LOCATION_HISTORY_ID))) %>% 
  #filter(grepl(ids, LOCATION_HISTORY_ID)) %>% 
  filter(INFORMATION_TYPE%in%c("RT","SR","SB","OW")) %>%
  filter(HS_HAB_STATUS %in% c("SUSPICIOUS","CONFIRMED", "CONFIRMED WITH HIGH TOXINS")) %>% distinct()  %>%
  select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID, SAMPLE_DATE, SAMPLE_TIME, INFORMATION_TYPE, SAMPLE_TYPE, DATA_PROVIDER, HS_HAB_STATUS, HS_HAB_STATUS_DATE, HS_HAB_STATUS_REMARK) %>%
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE, .keep_all = TRUE) %>% 
  mutate(Year=as.numeric(substr(SAMPLE_DATE,1,4))) 

habs <- habs %>% 
  group_by(LAKE_HISTORY_ID,Year) %>% 
  summarise(Date_First_Listing=min(SAMPLE_DATE),
            Date_Last_Listing=max(SAMPLE_DATE),
            Number_of_Reports=n(),
            List_of_Locations=paste0(unique(LOCATION_HISTORY_ID),collapse=", "))

cords<-newdata %>% filter(grepl("-",LOCATION_HISTORY_ID)|grepl("_C",LOCATION_HISTORY_ID)) %>% 
  select(LAKE_HISTORY_ID,LOCATION_PWL_ID,LAKE_WATERBODY_NAME,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% 
  distinct(LAKE_HISTORY_ID,.keep_all = TRUE)

habs<-merge(habs,cords,by=c('LAKE_HISTORY_ID'),all.x=TRUE)

habs<-habs %>% 
  rename(LATITUDE=LOCATION_Y_COORDINATE,
         LONGITUDE=LOCATION_X_COORDINATE) %>% 
  select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,Year,Date_First_Listing,Date_Last_Listing,Number_of_Reports,List_of_Locations,LATITUDE,LONGITUDE) %>% distinct()

write.csv(habs,file="junk.for.Karyn.csv",row.names=FALSE)
