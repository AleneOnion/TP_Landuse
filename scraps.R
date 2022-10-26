rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/Changepoint_disturbance.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/Changepoint_shoreline_disturbance.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/Changepoint_developed_only.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/Changepoint_developed_impairedTP.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/GAM_model.Rmd")


yuck<-newdata %>% 
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_VALIDATOR_QUALIFIER)&RSLT_VALIDATOR_QUALIFIER=="U",RSLT_METHOD_DETECTION_LIMIT/2,RSLT_RESULT_VALUE)) %>% 
  filter(CHARACTERISTIC_NAME=="PHOSPHORUS, TOTAL",
         !is.na(RSLT_RESULT_VALUE)) %>% 
  select(LAKE_HISTORY_ID) %>% distinct()
yuck<-merge(yuck,landcover_2016,by=c('LAKE_HISTORY_ID'),all.x=TRUE)
junk2<-yuck %>% filter(!is.na(DISTURBED)) %>% mutate(filter="ALL") %>% select(LAKE_HISTORY_ID,DISTURBED,filter) %>% distinct()


yuck<-newdata %>% 
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_VALIDATOR_QUALIFIER)&RSLT_VALIDATOR_QUALIFIER=="U",RSLT_METHOD_DETECTION_LIMIT/2,RSLT_RESULT_VALUE)) %>% 
filter(CHARACTERISTIC_NAME=="PHOSPHORUS, TOTAL",
                         !is.na(RSLT_RESULT_VALUE),
                         as.numeric(substr(SAMPLE_DATE,1,4))>'2010',
       RSLT_VALIDATOR_QUALIFIER!="R"|is.na(RSLT_VALIDATOR_QUALIFIER)) %>% 
  select(LAKE_HISTORY_ID) %>% distinct()
yuck<-merge(yuck,landcover_2016,by=c('LAKE_HISTORY_ID'),all.x=TRUE)
junk3<-yuck %>% filter(!is.na(DISTURBED))  %>% mutate(filter="recent") %>% select(LAKE_HISTORY_ID,DISTURBED,filter) %>% distinct()

yuck<-newdata %>% 
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_VALIDATOR_QUALIFIER)&RSLT_VALIDATOR_QUALIFIER=="U",RSLT_METHOD_DETECTION_LIMIT/2,RSLT_RESULT_VALUE)) %>% 
  filter(CHARACTERISTIC_NAME=="PHOSPHORUS, TOTAL",
         !is.na(RSLT_RESULT_VALUE),
         as.numeric(substr(SAMPLE_DATE,1,4))>'2010',
         INFORMATION_TYPE=="OW",
         RSLT_VALIDATOR_QUALIFIER!="R"|is.na(RSLT_VALIDATOR_QUALIFIER)) %>% 
  select(LAKE_HISTORY_ID) %>% distinct()
yuck<-merge(yuck,landcover_2016,by=c('LAKE_HISTORY_ID'),all.x=TRUE)
junk4<-yuck %>% filter(!is.na(DISTURBED)) %>% mutate(filter="surface") %>% select(LAKE_HISTORY_ID,DISTURBED,filter) %>% distinct()

yuck<-newdata %>% 
  filter(CHARACTERISTIC_NAME %in% c("PHOSPHORUS, TOTAL"),
         substr(SAMPLE_DATE,1,4)>'2010',
         INFORMATION_TYPE=="OW",
         !is.na(LAKE_HISTORY_ID)) %>% 
  mutate(depth=ifelse(!is.na(CSLAPFD_SITE_SOUND_DEPTH),CSLAPFD_SITE_SOUND_DEPTH,ifelse(!is.na(LCIFD_MAX_SOUND_DEPTH),LCIFD_MAX_SOUND_DEPTH,NA)))  %>%
  filter(RSLT_VALIDATOR_QUALIFIER!="R"|is.na(RSLT_VALIDATOR_QUALIFIER)) %>% 
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_VALIDATOR_QUALIFIER)&RSLT_VALIDATOR_QUALIFIER=="U",RSLT_METHOD_DETECTION_LIMIT/2,RSLT_RESULT_VALUE))%>%  
  filter(!is.na(RSLT_RESULT_VALUE)) %>%   
  mutate(phosphorus=1000*RSLT_RESULT_VALUE,
         depth=as.numeric(depth)) %>% 
  group_by(LAKE_HISTORY_ID) %>% 
  summarize(depth=max(depth),
            phosphorus=median(phosphorus)) %>% 
  ungroup()%>% 
  select(LAKE_HISTORY_ID,depth,phosphorus) %>% distinct() 
yuck<-merge(yuck,landcover_2016,by=c('LAKE_HISTORY_ID'),all.x=TRUE)
junk<-yuck %>% filter(!is.na(DISTURBED)) %>% mutate(filter="depth") %>% select(LAKE_HISTORY_ID,DISTURBED,filter) %>% distinct()

all<-merge(junk2,junk3,all=TRUE)
all<-merge(all,junk4,all=TRUE)
all<-merge(all,junk,all=TRUE)
ggplot(all, aes(x=DISTURBED,fill=filter)) + geom_histogram(position="dodge")
all %>% filter(DISTURBED<40) %>% 
  group_by(filter) %>% 
  summarize(n=n()) %>% 
  ungroup()
all %>% filter(DISTURBED>40) %>% 
  group_by(filter) %>% 
  summarize(n=n()) %>% 
  ungroup()


junk<-newdata %>% filter(LAKE_HISTORY_ID %in% c('1501LUC0982B','1306UWB5602','1501KAN1003','1301SIL0184C','1501LUC0982B','1303BRO0260'),LOCATION_TYPE=="CENTROID") %>% 
  select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()
