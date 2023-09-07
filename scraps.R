rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/Changepoint_disturbance.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/Changepoint_shoreline_disturbance.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/Changepoint_developed_only.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/Changepoint_developed_impairedTP.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/GAM_model.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/2023.03.GAM.models.Rmd")


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

junk<-read.csv("junk/Combined Analysis of ChlA (1).csv")
junk<-junk %>% select(LAKE_ID) %>% distinct() %>% rename(Lake_ID=LAKE_ID)
landcover_2016 <- read.csv("Statewide_Lake_Watershed_NLCD_2016_percentages_NHD_LakeID.csv")
junk<-merge(junk,landcover_2016,by=c('Lake_ID'),all.x=TRUE)
junk<-junk %>% select(Lake_ID,LkAcres) %>% distinct()

'0302SOD0096','0801SEC0782B'

lakes<-read.csv('trend_work_in_new_database/trend_lakes.csv')
lakes<-lakes %>% 
  filter(!is.na(PHOSPHORUS_OW_T)) %>% 
  select(LAKE_ID) %>% distinct() %>% rename(LAKE_HISTORY_ID=LAKE_ID) %>% 
  mutate(data_set="trend_lakes")
landcover_2016 <- read.csv("Statewide_Lake_Watershed_NLCD_2016_percentages_NHD_LakeID.csv")
landcover_2016<-landcover_2016 %>% 
  mutate(DEVELOPED=7.84*(VALUE_21+VALUE_22+VALUE_23+VALUE_24),
         CROPLAND=4.14*(VALUE_81+VALUE_82),
         GRASS_SHRUB=2.715*(VALUE_52+VALUE_71),
         BARREN=8.32*VALUE_31,
         FOREST=VALUE_41+VALUE_42+VALUE_43,
         WETLAND=VALUE_90+VALUE_95,
         LDI=DEVELOPED+CROPLAND+GRASS_SHRUB+BARREN+WETLAND+FOREST) %>% 
  select(Lake_ID,LDI,LkAcres) %>% distinct() %>% 
  mutate(LDI=as.numeric(LDI),
         LkAcres=as.numeric(LkAcres)) %>% 
  filter(LkAcres>6.4) %>% 
  rename('LAKE_HISTORY_ID'="Lake_ID") 
lakes<-merge(lakes,landcover_2016,by=c('LAKE_HISTORY_ID'),all=TRUE)
lakes<-lakes %>% filter(!is.na(LDI)) %>% 
  mutate(data_set=ifelse(is.na(data_set),"statewide",data_set))
trend_lakes<-lakes %>% 
  filter(data_set=="trend_lakes") %>% distinct() %>% 
  mutate(data_set="statewide")
lakes<-merge(lakes,trend_lakes,all=TRUE)
ggplot(lakes,aes(x=data_set,y=LDI, fill=data_set))+
  geom_boxplot()+
  theme(axis.title.x = element_blank())+
  ylab("Land Disturbance Index")

ggplot(lakes,aes(x=data_set,y=LkAcres, fill=data_set))+
  geom_boxplot()+
  theme(axis.title.x = element_blank())+
  ylim(0,400)+
  ylab("Lake Size (acres)")


lakes<-read.csv('trend_work_in_new_database/trend_lakes.csv')
lakes<-lakes %>% 
  filter(!is.na(PHOSPHORUS_OW_T)) %>% 
  select(LAKE_ID) %>% distinct() %>% rename(LAKE_HISTORY_ID=LAKE_ID) %>% 
  mutate(data_set="trend_lakes")
landcover_2016 <- read.csv("Statewide_Lake_Watershed_NLCD_2016_percentages_NHD_LakeID.csv")
landcover_2016<-landcover_2016 %>% 
  mutate(DEVELOPED=(VALUE_21+VALUE_22+VALUE_23+VALUE_24),
         CROPLAND=(VALUE_81+VALUE_82),
         GRASS_SHRUB=(VALUE_52+VALUE_71),
         BARREN=VALUE_31,
         FOREST=VALUE_41+VALUE_42+VALUE_43,
         WETLAND=VALUE_90+VALUE_95) %>% 
  select(Lake_ID,DEVELOPED) %>% distinct() %>% 
  mutate(DEVELOPED=as.numeric(DEVELOPED)) %>% 
  rename('LAKE_HISTORY_ID'="Lake_ID") 
lakes<-merge(lakes,landcover_2016,by=c('LAKE_HISTORY_ID'),all=TRUE)
lakes<-lakes %>% filter(!is.na(DEVELOPED)) %>% 
  mutate(data_set=ifelse(is.na(data_set),"statewide",data_set))
trend_lakes<-lakes %>% 
  filter(data_set=="trend_lakes") %>% distinct() %>% 
  mutate(data_set="statewide")
lakes<-merge(lakes,trend_lakes,all=TRUE)
ggplot(lakes,aes(x=data_set,y=DEVELOPED, fill=data_set))+
  geom_boxplot()+
  theme(axis.title.x = element_blank())+
  ylab("% Developed Land")

newdata %>% filter(LAKE_HISTORY_ID=="1306WALXXX1")

newdata %>% filter(LAKE_HISTORY_ID=="1301BRE0150D") %>% select(CHARACTERISTIC_NAME,SAMPLE_DATE,RSLT_RESULT_VALUE) %>% spread(CHARACTERISTIC_NAME,RSLT_RESULT_VALUE) %>% distinct()


newdata %>% filter(LAKE_HISTORY_ID=="0706OWA0212",CHARACTERISTIC_NAME=="PHOSPHORUS, TOTAL") %>% mutate(year=substr(SAMPLE_DATE,1,4)) %>% select(year,DATA_PROVIDER) %>% distinct()


rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend/Trend/logCHLA~logTP_onion.Rmd")


HAB_Samples<-newdata %>% 
  filter(LAKE_HISTORY_ID=="0906BLA0001",INFORMATION_TYPE!="RT",
         CHARACTERISTIC_NAME %in% c("CHLOROPHYLL A (PROBE) CONCENTRATION, CRYPTOPHYTA (CRYPTOPHYTES)","CYLINDROSPERMOPSIN","DOMINANT ALGAL SPECIES",
                                    "MICROCYSTIN","CHLOROPHYLL A (PROBE) CONCENTRATION, DINOPHYTA (DIATOMS)","CHLOROPHYLL A (PROBE) CONCENTRATION, CHLOROPHYTE (GREEN ALGAE)" ,
                                    "CHLOROPHYLL A (PROBE) CONCENTRATION, CYANOBACTERIA (BLUEGREEN)" ,"ANATOXIN-A","BMAA (BETA-METHYL-AMINO-(L)-ALANINE)",
                                    "CHLOROPHYLL A (PROBE) CONCENTRATION, TOTAL","MICROCYSTIN LA","MICROCYSTIN RR","MICROCYSTIN YR","MICROCYSTIN LR")) %>% distinct() %>% 
  rename(Water_Layer=INFORMATION_TYPE,
         Results=RSLT_RESULT_VALUE,
         Fraction=RSLT_RESULT_SAMPLE_FRACTION,
         Units=RSLT_RESULT_UNIT,
         Algal_Species=MR_RESULT_STRING,
         Location=LOCATION_HISTORY_ID) %>% 
  select(Location,SAMPLE_TIME,CHARACTERISTIC_NAME,Results,Water_Layer,Fraction,Units,Algal_Species) %>% 
  distinct() %>% 
  group_by(SAMPLE_TIME) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  filter(n>1) %>% select(-n)
HAB_Samples[is.na(HAB_Samples)] <- "" 
HAB_Status<-newdata %>% filter(LAKE_HISTORY_ID=="0906BLA0001") %>% distinct() %>% 
  rename(Bloom_Status=HS_HAB_STATUS,
         Status_Date=HS_HAB_STATUS_DATE,
         Record_DATE=SAMPLE_DATE,
         Location=LOCATION_HISTORY_ID) %>% 
  select(Location,Record_DATE,Status_Date,Bloom_Status) %>% distinct() 
HAB_Status[is.na(HAB_Status)] <- "" 

Water_Column<-newdata %>% filter(LAKE_HISTORY_ID=="0906BLA0001") %>% distinct() %>% 
  filter(INFORMATION_TYPE %in% c('OW','BS','SD')) %>%
  filter(CHARACTERISTIC_NAME %in% (c('QA','QB','QC','QD','QF'))==FALSE) %>%
  filter(!is.na(CHARACTERISTIC_NAME)) %>%
  select(LAKE_WATERBODY_NAME,LOCATION_NAME,'INFORMATION_TYPE',DATA_PROVIDER,SAMPLE_NAME,SAMPLE_DATE,SAMPLE_TIME,'SAMPLE_NAME',CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,RSLT_RESULT_SAMPLE_FRACTION)%>%
  #"PWLID","Waterbody_Classification","Beaches",PWS,LOCATION_ID,X_Coordinate,Y_Coordinate,
  rename(Water_Layer=INFORMATION_TYPE,
         Results=RSLT_RESULT_VALUE,
         Fraction=RSLT_RESULT_SAMPLE_FRACTION,
         Units=RSLT_RESULT_UNIT,
         Location=LOCATION_NAME) %>%
  mutate(Water_Layer=ifelse(Water_Layer=="OW","epilimnion",Water_Layer),
         Water_Layer=ifelse(Water_Layer=="BS","hypolimnion",Water_Layer),
         Water_Layer=ifelse(Water_Layer=="SD","not applicable",Water_Layer)) %>%
  arrange(SAMPLE_TIME,Location,CHARACTERISTIC_NAME,Fraction,Water_Layer) %>%
  select(SAMPLE_TIME,Location,CHARACTERISTIC_NAME,Water_Layer,Fraction,Units) %>% 
  distinct()
Water_Column[is.na(Water_Column)] <- "" 

newdata %>% filter(!is.na(LOCATION_PWL_ID),LAKE_WATERBODY_TYPE=="LAKE") %>% 
                     select(LAKE_HISTORY_ID,LOCATION_WATERBODY_CLASSIFICATION) %>% distinct() %>% mutate(LOCATION_WATERBODY_CLASSIFICATION=ifelse(grepl("A",LOCATION_WATERBODY_CLASSIFICATION),"A","not A")) %>% group_by(LOCATION_WATERBODY_CLASSIFICATION) %>% summarise(n=n()) %>% ungroup()
newdata %>% filter(LAKE_HISTORY_ID %in% c('0906BUT0054','0906GRA0051', '0303HYD0391','0906OTH0009','0906MIL0055','0905MOO0071'),LOCATION_TYPE=="CENTROID") %>% 
  select(LAKE_WATERBODY_NAME,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()

                   
newdata %>% filter(LAKE_HISTORY_ID %in% c('0906BUT0054','0906GRA0051', '0303HYD0391','0906OTH0009','0906MIL0055','0905MOO0071'),LOCATION_TYPE=="CENTROID") %>% 
  select(LAKE_WATERBODY_NAME,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()

newdata %>% filter(substr(SAMPLE_DATE,1,4)=="2022",DATA_PROVIDER=="LCI",CHARACTERISTIC_NAME=="NITROGEN, TOTAL") %>% select(LAKE_HISTORY_ID,SAMPLE_DATE) %>% distinct()


junk<-newdata %>% filter(substr(SAMPLE_DATE,1,4)=="2022",DATA_PROVIDER=="CSL",SAMPLE_TYPE=="WATER COLUMN") %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LAKE_WATERBODY_NAME) %>% distinct()


newdata %>% select(CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION) %>% distinct() %>% arrange(CHARACTERISTIC_NAME)

junk<-newdata %>% filter(PWS=="YES",LOCATION_TYPE=="CENTROID") %>% select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()

rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend/2023.01.03.lab.notebook.CSLAP.sample.number.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend/2023.01.03.lab.notebook.CSLAP.sample.number.all.lakes.Rmd")

newdata %>% filter(CHARACTERISTIC_NAME=="ARSENIC") %>% select(CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION,INFORMATION_TYPE) %>% distinct()


junk<-newdata %>% filter(substr(SAMPLE_DATE,1,4)=="2022",DATA_PROVIDER=="CSL") %>% select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME) %>% distinct()
junk1<-newdata %>% filter(substr(SAMPLE_DATE,1,4)=="2022",DATA_PROVIDER=="LCI") %>% select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME) %>% distinct()
junk3<-merge(junk,junk1)


junk<-newdata %>% filter(substr(SAMPLE_DATE,1,4)=='2022',DATA_PROVIDER=="CSL",CHARACTERISTIC_NAME=="PHOSPHORUS, TOTAL", !is.na(RSLT_RESULT_VALUE)) %>% distinct()

ggplot(df %>% filter(Parameter=="Color",LAKE_HISTORY_ID=="0402CON0067"), aes(x=SAMPLE_DATE,y=Result)) + 
  geom_point()

junk<-df %>% filter(Parameter=="Color",LAKE_HISTORY_ID=="0602BRA0154")

junk<-newdata %>% 
  filter(CHARACTERISTIC_NAME=="TRUE COLOR",DATA_PROVIDER=="CSL",RSLT_RESULT_VALUE==1) %>% distinct() %>% 
  mutate(year=substr(SAMPLE_DATE,1,4)) %>% 
  group_by(year) %>% 
  summarise(n=n())

rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend/Trend/logCHLA~logTP_onion.Rmd")


newdata %>% filter(CHARACTERISTIC_NAME=="TRUE COLOR",RSLT_RESULT_VALUE=='1') %>% select(RSLT_LABORATORY_QUALIFIER) %>% distinct()


junk<-newdata %>% filter(!is.na(RSLT_RESULT_VALUE),LAKE_WATERBODY_TYPE=="LAKE") %>% select(LAKE_HISTORY_ID) %>% distinct()
junk$randoms<-runif(1231,min=0,max=1)
junk<-junk %>% arrange(randoms)
junk<-junk[1:124,1]
junk <- data.frame(junk)

folder_list <- list.files('C:/Users/amonion/New York State Office of Information Technology Services/BWAM - LCI reports/automated_reports/2022')
folder_list <- data.frame(folder_list)
folder_list<-folder_list %>% 
  mutate(folder_list=gsub(".html","",folder_list),
         folder_list=gsub(".*_","",folder_list),
         present="yes")


rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/2023.03.GAM.models.Rmd")


#################################################################################################################################
#################################################################################################################################
#################################################################################################################################
#################################################################################################################################
#Identifying CSLAP participants who can answer whether lake has been treated with algicide in past decade
#################################################################################################################################
#################################################################################################################################
#################################################################################################################################
junk<-df_slope %>% filter(Parameter %in% c('ChlA','TP')) %>% 
  mutate(slope=ifelse(is.na(slope),0,slope)) %>% 
  select(LAKE_HISTORY_ID,Parameter,slope) %>% distinct() %>% 
  spread(Parameter,slope) %>% 
  filter(ChlA<0,TP==0|TP>0)
cslap<-newdata %>% select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LAKE_PROJ_CODE) %>% filter(!is.na(LAKE_PROJ_CODE)) %>% distinct()
junk<-merge(junk,cslap,by=c('LAKE_HISTORY_ID'),all.x=TRUE)
junk<-junk %>% 
  mutate(LAKE_PROJ_CODE=ifelse(LAKE_HISTORY_ID=="1104SAC0314",27,LAKE_PROJ_CODE),
         LAKE_PROJ_CODE=ifelse(LAKE_HISTORY_ID=="1104SCH0374",34,LAKE_PROJ_CODE),
         LAKE_WATERBODY_NAME=ifelse(LAKE_HISTORY_ID=="1104SAC0314","SACANDAGA LAKE",LAKE_WATERBODY_NAME),
         LAKE_WATERBODY_NAME=ifelse(LAKE_HISTORY_ID=="1104SCH0374","SCHROON LAKE",LAKE_WATERBODY_NAME))
training<-read.csv("C:/Users/amonion/New York State Office of Information Technology Services/LMAS - CSLAP/Operations/2023/2023 Master CSLAP Volunteer List FINAL_most_recent.csv")
training<-training %>% 
  rename(LAKE_PROJ_CODE=Lake.,
         name=X.2,
         email=X.3,
         phone=X.8) %>% 
  filter(!is.na(email),email!="",!is.na(LAKE_PROJ_CODE)) %>% 
  select(LAKE_PROJ_CODE,name,email) %>% distinct()
junk<-merge(junk,training,by=c('LAKE_PROJ_CODE'),all.x=TRUE)
junk<-junk %>% 
  mutate(LAKE_WATERBODY_NAME=ifelse(LAKE_HISTORY_ID=="1104GOO0672A","GOODNOW FLOWAGE",LAKE_WATERBODY_NAME))
write.csv(junk,file="C:/Users/amonion/New York State Office of Information Technology Services/LMAS - CSLAP/Operations/2023/contacting.about.algicides.for.trend.csv",row.names=FALSE)
#################################################################################################################################
#################################################################################################################################
#################################################################################################################################
#################################################################################################################################


rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/2023.03.GAM.models.Rmd")

junk<-TP_matrix %>% select(LAKE_HISTORY_ID,phosphorus,septic) %>% distinct() 
print(ggplot(junk, aes(x=septic,y=phosphorus))+
        geom_point()+
        geom_smooth()+
        geom_hline(yintercept=20)+
        ylim(0,50))


mcycle <- MASS::mcycle

library(mgcv)
# Fit the model
mod <- gam(accel ~ s(times), data = mcycle, method = "REML")

# Make the plot with residuals
plot(mod, residuals=TRUE)
plot(mod, residuals=TRUE,pch=1,cex=1)


library(gamair)
data("mpg", package="gamair")

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")
plot(mod,select = 3)
plot(mod,pages=1,all.terms=TRUE,residuals=TRUE)
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink")
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1], seWithMean = TRUE)


library(gamair)
set.seed(0)
data("mpg", package="gamair", verbose=FALSE)

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")
concurvity(mod, full = TRUE)


lakedepth <- newdata%>%
  select(LAKE_HISTORY_ID,CSLAPFD_SITE_SOUND_DEPTH,RSLT_PROFILE_DEPTH,LCIFD_MAX_SOUND_DEPTH,LCIFD_SITE_SOUND_DEPTH) %>% distinct()
junk<-TP_matrix %>% filter(depth==-Inf) %>% select(LAKE_HISTORY_ID,depth) %>% distinct()
lakedepth<-merge(junk,lakedepth,by=c("LAKE_HISTORY_ID"),all.x=TRUE)
lakedepth<-lakedepth %>% 
  mutate(CSLAPFD_SITE_SOUND_DEPTH=gsub("m","",CSLAPFD_SITE_SOUND_DEPTH),
         CSLAPFD_SITE_SOUND_DEPTH=gsub(" ","",CSLAPFD_SITE_SOUND_DEPTH),
         RSLT_PROFILE_DEPTH=gsub("m","",RSLT_PROFILE_DEPTH),
         RSLT_PROFILE_DEPTH=gsub(" ","",RSLT_PROFILE_DEPTH),
         LCIFD_MAX_SOUND_DEPTH=gsub("m","",LCIFD_MAX_SOUND_DEPTH),
         LCIFD_MAX_SOUND_DEPTH=gsub(" ","",LCIFD_MAX_SOUND_DEPTH),
         LCIFD_SITE_SOUND_DEPTH=gsub("m","",LCIFD_SITE_SOUND_DEPTH),
         LCIFD_SITE_SOUND_DEPTH=gsub(" ","",LCIFD_SITE_SOUND_DEPTH)) %>% 
  mutate(CSLAPFD_SITE_SOUND_DEPTH=as.numeric(CSLAPFD_SITE_SOUND_DEPTH),
         RSLT_PROFILE_DEPTH=as.numeric(RSLT_PROFILE_DEPTH),
         LCIFD_MAX_SOUND_DEPTH=as.numeric(LCIFD_MAX_SOUND_DEPTH),
         LCIFD_SITE_SOUND_DEPTH=as.numeric(LCIFD_SITE_SOUND_DEPTH)) %>%   
  group_by(LAKE_HISTORY_ID) %>%
  summarize(CSLAPFD_SITE_SOUND_DEPTH=max(CSLAPFD_SITE_SOUND_DEPTH,na.rm = TRUE),
            LCIFD_MAX_SOUND_DEPTH=max(LCIFD_MAX_SOUND_DEPTH,na.rm = TRUE),
            LCIFD_SITE_SOUND_DEPTH=max(LCIFD_SITE_SOUND_DEPTH,na.rm = TRUE),
            RSLT_PROFILE_DEPTH=max(RSLT_PROFILE_DEPTH,na.rm = TRUE)) %>%
  ungroup() %>%
  gather(type,depth,-LAKE_HISTORY_ID) %>% 
  mutate(depth=as.numeric(depth)) %>% 
  group_by(LAKE_HISTORY_ID) %>% 
  summarise(depth=max(depth)) %>% 
  ungroup() %>% 
  mutate(depthe=ifelse(depth==-Inf,NA,depth))


 setwd("C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Lakes Database/Current")

 
 
 
 ## Correlation with Chloride 
 
 ```{r, class.source = 'fold-show'}
 Cl<-Chloride %>% rename(LAKE_ID=LAKE_HISTORY_ID,Cl=Result) %>% select(LAKE_ID,SAMPLE_DATE,Cl) %>% distinct()
 Cl<-merge(df,Cl,by=c('LAKE_ID','SAMPLE_DATE'),all.x=TRUE)
 Cl<-Cl %>% select(LAKE_ID,SAMPLE_DATE,SPC,Cl) %>% 
   filter(!is.na(Cl),!is.na(SPC))
 ```
 
 ```{r, class.source = 'fold-show'}
 #linear model of chlorophyll to phosphorus
 lmCl <- lm(SPC~Cl, data=Cl)
 summary(lmCl)
 ```
 
 ```{r plot}
 print(ggplot(Cl,aes(x=Cl,y=SPC))+
         geom_point()+
         geom_smooth(method='lm')+
         labs(title="Linear SPC~Cl model",x="Cl",y="SPC"))
 ```
 
 
 rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend/Trend/logCHLA~logTP_onion.Rmd")
 
 print(ggplot(acid,aes(x=acid,y=log(`N:P`)))+
         geom_point()+
         geom_smooth(method='lm')+
         geom_hline(yintercept=0.5)+
         labs(title="acid deposition",x="Wet N Deposition",y="N:P"))
 
 
 
 not_any_na <- function(x) all(!is.na(x))
junk<- junk %>% select(where(not_any_na)) 
1501DEF0977A

rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/2023.03.GAM.models.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/2023.07.GAM.models.Rmd")
rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/TP_Landuse/2023.07.27.GAM.models.Rmd")





fringe<-TP_matrix %>% select(LAKE_HISTORY_ID,urban_fringe) %>% distinct()
landcover <- read.csv("NLCD_2019_Lake_Watershed.csv")
landcover<-landcover %>% 
  mutate_at(c('Open.Water','Developed..Open.Space', 'Developed..Low.Intensity','Developed..Medium.Intensity.','Developed..High.Intensity','Barren.Land','Deciduous.Forest','Evergreen.Forest','Mixed.Forest','Shrub.Scrub',
              'Herbaceous','Hay.Pasture','Cultivated.Crops','Woody.Wetlands','Emergent.Herbaceous.Wetlands'),as.numeric) %>% 
  rename(LAKE_HISTORY_ID = LAKE_ID) %>% 
  filter(LAKE_HISTORY_ID!=" ") %>% 
  mutate(#note that the 2013 article did not use 81 (pasture hay) but we include it
    totalall=Developed..Open.Space+Developed..Low.Intensity+Developed..Medium.Intensity.+Developed..High.Intensity+
      Barren.Land+Deciduous.Forest+Evergreen.Forest+Mixed.Forest+Shrub.Scrub+Herbaceous+
      Hay.Pasture+Cultivated.Crops+Woody.Wetlands+Emergent.Herbaceous.Wetlands,
    Developed..Open.Space=Developed..Open.Space/totalall,
    Developed..Low.Intensity= Developed..Low.Intensity/totalall,
    Developed..Medium.Intensity.=Developed..Medium.Intensity./totalall,
    Developed..High.Intensity=Developed..High.Intensity/totalall,
    Barren.Land=Barren.Land/totalall,
    Deciduous.Forest=Deciduous.Forest/totalall,
    Evergreen.Forest=Evergreen.Forest/totalall,
    Mixed.Forest=Mixed.Forest/totalall,
    Shrub.Scrub=Shrub.Scrub/totalall,
    Herbaceous=Herbaceous/totalall,
    Hay.Pasture=Hay.Pasture/totalall,
    Cultivated.Crops=Cultivated.Crops/totalall,
    Woody.Wetlands=Woody.Wetlands/totalall,
    Emergent.Herbaceous.Wetlands=Emergent.Herbaceous.Wetlands/totalall) %>% 
  distinct(LAKE_HISTORY_ID,Developed..Open.Space,Developed..Low.Intensity,Developed..Medium.Intensity.,Developed..High.Intensity,
             Barren.Land,Deciduous.Forest,Evergreen.Forest,Mixed.Forest,Shrub.Scrub,Herbaceous,
             Hay.Pasture,Cultivated.Crops,Woody.Wetlands,Emergent.Herbaceous.Wetlands,.keep_all = TRUE) 
fringe<-merge(fringe,landcover,by=c('LAKE_HISTORY_ID'),all.x=TRUE)
fringe<-fringe %>% 
  select(-totalall,-Open.Water) %>% 
  pivot_longer(cols=Developed..Open.Space:Emergent.Herbaceous.Wetlands,
               names_to='land_use',values_to="pct") %>% 
  distinct()
library(ggplot2)
ggplot(fringe,aes(x=land_use,y=pct,color=urban_fringe))+
         geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



newdata %>% filter(LAKE_HISTORY_ID=="1302WAC0117",CHARACTERISTIC_NAME=="CALCIUM") %>% 
  select(SAMPLE_DATE,RSLT_R,LOCATION_NAME=="CENTROID")ESULT_VALUE,RSLT_VALIDATOR_QUALIFIER) %>% distinct() %>% arrange(SAMPLE_DATE)

library(tidyverse)
junk<-newdata %>% 
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_VALIDATOR_QUALIFIER)&RSLT_VALIDATOR_QUALIFIER=="U",RSLT_METHOD_DETECTION_LIMIT/2,RSLT_RESULT_VALUE)) %>% 
  filter(CHARACTERISTIC_NAME %in% c("CHLOROPHYLL A (PROBE)","CHLOROPHYLL A"),
         !is.na(RSLT_RESULT_VALUE),
         as.numeric(substr(SAMPLE_DATE,1,4))>'2010',
         RSLT_VALIDATOR_QUALIFIER!="R"|is.na(RSLT_VALIDATOR_QUALIFIER)) %>% 
  mutate(INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="OW","epilimnion sample",ifelse(INFORMATION_TYPE=="DP","depth profile", ifelse(INFORMATION_TYPE=="BS","hypolimnion sample",INFORMATION_TYPE)))) %>% 
  select(LAKE_WATERBODY_NAME,LOCATION_NAME,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,SAMPLE_DATE,INFORMATION_TYPE,CHARACTERISTIC_NAME,RSLT_PROFILE_DEPTH,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT,WCFD_EQUIPMENT_TYPE,WCFD_EQUIPMENT_DESC) %>% distinct()


junk<-newdata %>% 
  filter(LAKE_WATERBODY_TYPE=="LAKE") %>% select(LAKE_HISTORY_ID) %>% distinct()
junk<-newdata %>% filter(LAKE_HISTORY_ID %in% (junk$LAKE_HISTORY_ID),LOCATION_TYPE=="CENTROID") %>% 
  select(LAKE_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% 
  distinct(LAKE_HISTORY_ID,.keep_all = TRUE)



rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend/2023.08.lab.notebook.plotting.all.chla.TP.Rmd")

rmarkdown::render("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend/2023.09.06.GAM.models.Rmd")
