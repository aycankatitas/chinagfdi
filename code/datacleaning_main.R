##################
# China GFDI MA paper 
# Date: 1/20/2023 
# Author: Aycan Katitas 
# Data Cleaning and Merging - for R&R
##################

##################
### setwd and load libraries 

library(readstata13)
library(dplyr)
library(foreign)
library(lubridate)
library(tidyverse)
library(totalcensus)
library(censusapi)
library(broom)
library(scales)
library(quantmod)
##################


##################
# Functions 

correct_adminregion <- function(data){
  
  orx <- c("New York City County","San Francisco City & County","City of Danville County","City of Suffolk County",
           "Lexington-Fayette County","Denver City and County","Louisville Jefferson County","City of Virginia Beach County",
           "Seminole County","Lafayette Consolidated Government Parish","Unified Govt. of Wyandotte County","City of Norfolk County",
           "La Grange County","City of Newport News County","City of Bristol County","City of Charlottesville County",
           "City of Hopewell County","Broomfield City and County","Columbus-Muscogee County","City of Chesapeake County",
           "City of Martinsville County","City of Covington County","Ste. Genevieve County","St John The Baptist Parish",
           "Camden  County","Athens-Clarke County","La Porte County","City of Salem County","City of Lynchburg County",
           "City of Fredericksburg","City of Galax County","City of Waynesboro","City of Franklin County","Carson City",
           "City of Colonial Heights County","Wayne  County","Lewis And Clark County","Orleans County",
           "City of Emporia County","NevadaCounty","La PazCounty","King And Queen County","Honolulu City and County County",
           "DeSoto Parish"
  )
  
  tarx <- c("New York County","San Francisco County","Danville City","Suffolk City",
            "Fayette County","Denver County","Jefferson County","Virginia Beach City",
            "Seminole County","Lafayette Parish","Wyandotte County","Norfolk City",
            "LaGrange County","Newport News City","Bristol City","Charlottesville City",
            "Hopewell City","Broomfield County","Muscogee County","Chesapeake City",
            "Martinsville City","Covington City","Ste Genevieve County","St John the Baptist Parish",
            "Camden County","Clarke County","LaPorte County","Salem City","	Lynchburg City",
            "Fredericksburg City","Galax City","Waynesboro City","Franklin City","Carson City City",
            "Colonial Heights City","Wayne County","Lewis and Clark County","Orleans Parish",
            "Emporia City","Nevada County","La Paz County","King and Queen County","Honolulu County",
            "De Soto Parish")
  
  data$County <- mgsub::mgsub(data$County, orx,tarx)
  
  data
}

# Unemployment 
read_unemp <- function(filename,year){
  
  unemp <- read.fwf(
    skip=6,
    file=filename,
    widths=c(17, 7, 7, 47, 12,11,14,9, 8), stringsAsFactors=FALSE)
  
  unemp <- head(unemp,-3)
  
  unemp <- unemp %>%
    dplyr::select(V2,V3,V6,V8) %>% 
    rename(statefips=V2,
           countyfips=V3,
           labforce=V6,
           unemp=V8) %>% 
    mutate(statefips=trimws(statefips),
           countyfips=as.character(sprintf("%03d",countyfips)),
           FIPS=as.numeric(paste0(statefips,countyfips)),
           labforce=as.numeric(trimws(gsub(",","",labforce))),
           unemp=as.numeric(trimws(gsub(",","",unemp)))) %>%
    dplyr::select(FIPS,labforce,unemp) %>% 
    rename("unemp_{{year}}" := unemp) %>% 
    rename("labforce_{{year}}" := labforce )
  
  
  return(unemp)
}

# Log of vars that contain zero 
logplus <- function(x){
  d <- log(x+0.1)
}

#####################

#####################
# Set parameters 

# Configuring dates & COO 
beforedate = "2012-09-01"
afterdate = "2003-01-01"
origin="China"

#origin="notchina"
#origin="all"
#origin= c("Japan","South Korea","Vietnam")

#Configuring census variables
selectvars <- c("NAME","B01003_001E","B19019_001E","B05006_049E","B05006_047E","B05006_001E",
                "B08126_004E","B08126_001E","B06009_005E","B06009_006E","B06009_001E"
)



#mycensuskey <- [YOUR CENSUS API KEY HERE]

## ANES 2012 and ANES 2020 

#anesfull <- read.dta13("anes_timeseries_2012.dta")
anesfull <- read.dta("anes_timeseries_2012_Stata12.dta")
anes <- read.dta("anes_timeseries_2012_Stata12.dta",convert.factors=FALSE)

# f <- anesfull %>% 
#   dplyr::select(caseid,sample_stfips,sample_district,sample_district_prev)

# anes <- left_join(anes,f)

anes <- anes %>% 
  mutate(cd=ifelse(sample_district<0,NA,sample_district),
         cd_prev=ifelse(sample_district_prev<0,NA,sample_district_prev),
         cd_prev=ifelse(sample_state=="AK"|sample_state=="DE"|sample_state=="MT"|sample_state=="ND"|sample_state=="SD"|
                          sample_state=="VT"|sample_state=="WY"|sample_state=="DC",1,cd_prev), 
         cdids_prev=paste0(sample_state,cd_prev),
         cd=ifelse(sample_state=="AK"|sample_state=="DE"|sample_state=="MT"|sample_state=="ND"|sample_state=="SD"|
                     sample_state=="VT"|sample_state=="WY"|sample_state=="DC",1,cd),
         cdids=paste0(sample_state,cd))

# f <- anes %>% 
#   mutate(chinaect=ifelse(china_econ<0,NA,china_econ)) %>% 
#   count(chinaect,wt=weight_full)


# coding
anes2 <- anes %>% 
  mutate(chinaect=ifelse(china_econ==1|china_econ==3,0,
                         ifelse(china_econ==2,1,NA)),
         chinaect2=ifelse(china_econ==1,0,
                          ifelse(china_econ==3,1,
                                 ifelse(china_econ==2,2,NA))),
         chinaect3=ifelse(china_econ==1,0,
                          ifelse(china_econ==2|china_econ==3,1,NA)),
         chinadk=ifelse(china_econ==-8,1,
                        ifelse(china_econ>0,0,NA)),
         chinaord=ifelse(china_econ==1,0,
                         ifelse(china_econ==3,1,
                                ifelse(china_econ==2,2,NA))),
         chinamil=ifelse(china_mil==1,2,
                         ifelse(china_mil==2,1,
                                ifelse(china_mil==3,0,NA))),
         china_sd=rescale(chinaord),
         natcontt=ifelse(patriot_amident<0,NA,patriot_amident),
         nationalism=abs(natcontt-5),
         econoutlook2= ifelse(econ_ecnext_x<0,NA,econ_ecnext_x),
         econoutlook= econoutlook2-1,
         party2=ifelse(pid_x<0,NA,pid_x),
         party=party2-1,
         education2=ifelse(dem_edugroup_x<0,NA,dem_edugroup_x),
         education=education2-1,
         age=ifelse(dem_age_r_x<0,NA,dem_age_r_x),
         gender=gender_respondent_x-1,
         race=ifelse(dem_raceeth_x<0,NA,
                     ifelse(dem_raceeth_x==3|dem_raceeth_x==4|dem_raceeth_x==6,4,
                            ifelse(dem_raceeth_x==5,3,dem_raceeth_x))),
         income_cont = ifelse(incgroup_prepost_x==1|incgroup_prepost_x==2,1,
                              ifelse(incgroup_prepost_x==3|incgroup_prepost_x==4,2,
                                     ifelse(incgroup_prepost_x==5|incgroup_prepost_x==6,3,
                                            ifelse(incgroup_prepost_x==7|incgroup_prepost_x==8,4,
                                                   ifelse(incgroup_prepost_x==9|incgroup_prepost_x==10,5,
                                                          ifelse(incgroup_prepost_x==11,6,
                                                                 ifelse(incgroup_prepost_x==12,7,
                                                                        ifelse(incgroup_prepost_x==13,8,
                                                                               ifelse(incgroup_prepost_x==14,9,
                                                                                      ifelse(incgroup_prepost_x==15|incgroup_prepost_x==16,10,
                                                                                             ifelse(incgroup_prepost_x==17,11,
                                                                                                    ifelse(incgroup_prepost_x==18,12,
                                                                                                           ifelse(incgroup_prepost_x==19,13,
                                                                                                                  ifelse(incgroup_prepost_x==20,14,
                                                                                                                         ifelse(incgroup_prepost_x==21,15,
                                                                                                                                ifelse(incgroup_prepost_x==22,16,
                                                                                                                                       ifelse(incgroup_prepost_x==23,17,
                                                                                                                                              ifelse(incgroup_prepost_x==24,18,
                                                                                                                                                     ifelse(incgroup_prepost_x==25,19,
                                                                                                                                                            ifelse(incgroup_prepost_x==26,20,
                                                                                                                                                                   ifelse(incgroup_prepost_x==27,21,
                                                                                                                                                                          ifelse(incgroup_prepost_x==28,22,NA)))))))))))))))))))))),
         income_grp=ifelse(incgroup_prepost_x>0&incgroup_prepost_x<7,1,
                           ifelse(incgroup_prepost_x>6&incgroup_prepost_x<19,2,
                                  ifelse(incgroup_prepost_x>18&incgroup_prepost_x<26,3,
                                         ifelse(incgroup_prepost_x>25,4,NA)))),
         ideology=ifelse(libcpre_self<0,NA,libcpre_self)) %>% 
  # creating the media variable - days out of the week
  ## internet
  mutate(
    inmedia = ifelse(prmedia_wkinews < 0, NA, prmedia_wkinews),
    at_inmedia = ifelse(prmedia_atinews < 0 & prmedia_wkinews != 0,NA,
                        ifelse(prmedia_wkinews == 0, 0, prmedia_atinews)),
    w_inmedia = inmedia * at_inmedia,
    ## TV
    tvmedia = ifelse(prmedia_wktvnws < 0, NA, prmedia_wktvnws),
    at_tvmedia = ifelse(prmedia_attvnews < 0 & prmedia_wktvnws != 0,NA,
                        ifelse(prmedia_wktvnws == 0, 0, prmedia_attvnews)),
    w_tvmedia = tvmedia * at_tvmedia,
    ## Print media
    prmedia = ifelse(prmedia_wkpaprnws < 0, NA, prmedia_wkpaprnws),
    at_prmedia = ifelse(prmedia_atpprnews < 0 & prmedia_wkpaprnws != 0,NA,
                        ifelse(prmedia_wkpaprnws == 0, 0, prmedia_atpprnews)),
    w_prmedia = prmedia * at_prmedia,
    ## Radio
    rdmedia = ifelse(prmedia_wkrdnws < 0, NA, prmedia_wkrdnws),
    at_rdmedia = ifelse(prmedia_atrdnews < 0 & prmedia_wkrdnws != 0,NA,
                        ifelse(prmedia_wkrdnws == 0, 0, prmedia_atrdnews)),
    w_rdmedia = rdmedia * at_rdmedia) %>% 
  # overall media sum 
  rowwise() %>% 
  mutate(wimediasum = sum(w_inmedia,w_tvmedia,w_prmedia,w_rdmedia, na.rm=TRUE),
         woimediasum=sum(w_tvmedia,w_prmedia,w_rdmedia, na.rm=TRUE),
         wimediasum_av=wimediasum/4,
         woimediasum_av=woimediasum/3) %>% 
  dplyr::select(caseid,sample_state,sample_district,cdids,cdids_prev,starts_with("china"),
                nationalism,econoutlook,party,education,age,gender,race,income_grp,income_cont,ideology,
                ends_with("media"),-medsrc_socmedia,-modsex_media,
                -paprofile_freqpolit_socmedia,-paprofile_politinfo_socmedia,
                wimediasum_av,woimediasum_av)

anesnewscons <- anes2 %>% 
  dplyr::select(caseid,ends_with("media"),wimediasum_av,woimediasum_av)

saveRDS(anesnewscons,file="anes12newscons.Rds")
# Occupation 

anesocc <- read.csv("anesocc.csv",sep=";",na.strings=c("","NA"),stringsAsFactors = FALSE)

# determine who real missing are 

m <- anesfull %>% 
  filter(dem_empstatus_2digitfin_x=="-9. Refused"|dem_empstatus_2digitfin_x=="-8. Don't know")


anesocc2 <- anesocc %>%
  dplyr::select(caseid, dem_occpast, dem_indpast, dem_occnow, dem_indnow) 
anesocc2$dem_occnow <- enc2utf8(anesocc2$dem_occnow)
anesocc2$dem_occpast <- enc2utf8(anesocc2$dem_occpast)
anesocc2$dem_indnow <- enc2utf8(anesocc2$dem_indnow)
anesocc2$dem_indpast <- enc2utf8(anesocc2$dem_indpast)


anesocc2 <- anesocc2 %>% 
  mutate(dem_occnow=tolower(dem_occnow),
         dem_occpast=tolower(dem_occpast),
         dem_indnow=tolower(dem_indnow),
         dem_indpast=tolower(dem_indpast))

anesocc3 <- anesocc2 %>% 
  mutate(manufnow=ifelse(grepl("manu",dem_occnow)&!grepl("manuver",dem_occnow),1,
                         ifelse(grepl("manu",anesocc2$dem_indnow),1,
                                ifelse(caseid%in%m$caseid,NA,0))),
         manufall=ifelse(grepl("manu",dem_occnow)&!grepl("manuver",dem_occnow),1,
                         ifelse(grepl("manu",dem_occpast)&!grepl("manuver",dem_occpast),1,
                                ifelse(grepl("manu",anesocc2$dem_indnow)|grepl("manu",anesocc2$dem_indpast),1,
                                       ifelse(caseid%in%m$caseid,NA,0)))))%>% 
  dplyr::select(caseid,starts_with("manuf"))

anes12 <- left_join(anes2,anesocc3)


saveRDS(anesocc3,file="anes12occdata.Rds")

anes12$year=2012


# ANES 2020 
# V200001 case IDs, V160001_orig 2016 resampled peeps IDs 
# V200010a - full sample pre-election V200010b post election weights
# nationalism qs higher, more nationalist


anes20full <- read.dta13("anes_timeseries_2020_stata_20220210.dta")

anes20 <- read.dta13("anes_timeseries_2020_stata_20220210.dta",convert.factors=FALSE)

# create variables 
#chinathreat 0 No 1 Yes
# american nationalism vars - higher more nationalistic 
# econoutlook higher - much worse
# party - Democrats to Reps 
# education - No - Grad 
# gender 0 Male 1 Female

# f <- anes20full %>%
#   dplyr::select(V200001,V202400,V200010a) %>%
#   mutate(chinathreat=ifelse(V202400<0,NA,V202400)) %>%
#   count(chinathreat,wt=V200010a)
anes20v <- anes20full %>%
  dplyr::select(V200001, V200010a,V200010b,V203001,V203002,V202400,V202401,V202402,V202403,V202404,V202405,V202424,V202504,
                V202565,V202566,V202270,V201330x,V201231x,V201511x,V201507x,V201600,V202407,V201549x,V202468x,V201200) %>% 
  mutate(chinathreat=ifelse(V202400<0,NA,
                            ifelse(V202400<4,0,1)),
         russiathreat=ifelse(V202401<0,NA,
                             ifelse(V202401<4,0,1)),
         mexicothreat=ifelse(V202402<0,NA,
                             ifelse(V202402<4,0,1)),
         iranthreat=ifelse(V202403<0,NA,
                           ifelse(V202403<4,0,1)),
         japanthreat=ifelse(V202404<0,NA,
                            ifelse(V202404<4,0,1)),
         germanythreat=ifelse(V202405<0,NA,
                              ifelse(V202405<4,0,1)),
         amident2=ifelse(V202504<0,NA,V202504),
         amident=abs(amident2-5),
         americacustom2=ifelse(V202424<0,NA,V202424),
         americacustom=abs(americacustom2-5),
         worldamer2=ifelse(V202270<0,NA,V202270),
         worldamer=abs(worldamer2-5),
         econoutlook2=ifelse(V201330x<0,NA,V201330x),
         econoutlook=econoutlook2-1,
         party2=ifelse(V201231x<0,NA,V201231x),
         party=party2-1,
         education2=ifelse(V201511x<0,NA,V201511x),
         education=education2-1,
         age=ifelse(V201507x<0,NA,V201507x),
         gender2=ifelse(V201600<0,NA,V201600),
         gender=gender2-1,
         chinaord2=ifelse(V202400<0,NA,V202400),
         chinaord=chinaord2-1,
         china_sd=rescale(chinaord),
         chinaect2=ifelse(V202400==1|V202400==2,0,
                          ifelse(V202400==3,1,
                                 ifelse(V202400==4|V202400==5,2,NA))),
         chinaect3=ifelse(V202400<0,NA,
                                 ifelse(V202400<3,0,1)),
         # media 3- very closely follow 0 - not at all closely 
         mediaat2=ifelse(V202407<0,NA,V202407),
         mediaat=abs(mediaat2-4),
         race=ifelse(V201549x<0,NA,
                     ifelse(V201549x>3,4,V201549x)),
         income_cont=ifelse(V202468x<0,NA,V202468x),
         income_grp=ifelse(V202468x>0&V202468x<4,1,
                           ifelse(V202468x>3&V202468x<13,2,
                                  ifelse(V202468x>12&V202468x<20,3,
                                         ifelse(V202468x>19,4,NA)))),
         ideology=ifelse(V201200<0,NA,V201200))

# case id,pre-weight,post-weight, state, cdid, china a threat, russia threat, mexico threat, iran threat, japan threat, germany threat,
#how important to follow America's customs, choosing products made in america, display american flag, better place if other countries more like Americans, econ outlook future, party,
# education, age, gender

anes20v$nationalism <- anes20v$amident
anes20v$year = 2020

anes20v <- anes20v %>%
  rename(caseid=V200001,
         sample_state=V203001,
         sample_district=V203002,
         chinaect=chinathreat) %>% 
  mutate(sample_district=ifelse(sample_state=="AK"|sample_state=="DE"|sample_state=="MT"|sample_state=="ND"|sample_state=="SD"|
                         sample_state=="VT"|sample_state=="WY"|sample_state=="DC",1,sample_district),
    cdids=paste0(sample_state,sample_district),
         cdids_prev=cdids)


anesnewscons20 <- anes20v %>% 
  dplyr::select(caseid,mediaat)

saveRDS(anesnewscons20,file="anes20newscons.Rds")

# Create time series anes data 
anes12t <- anes12 %>% 
  dplyr::select(year,caseid,sample_state,sample_district,cdids,cdids_prev,chinaect,chinaect2,chinaect3,
                income_grp,income_cont,ideology,
                nationalism,econoutlook,education,party,age,gender,race,chinaord,china_sd,chinamil)

anes20t <- anes20v %>% 
  mutate(chinamil=NA) %>% 
  dplyr::select(year,caseid,sample_state,sample_district,cdids,cdids_prev,chinaect,chinaect2,chinaect3,
                income_grp,income_cont,ideology,
                nationalism,econoutlook,education,party,age,gender,race,chinaord,china_sd,chinamil)

anestime <- rbind(anes12t,anes20t)


##################
### Chinese FDI 
## Greenfield 

#Combine 2003-2019 and 2019-2021 data together 
load("fdi0319.Rdata")

names(fdi) <- c("Date","investing.comp","parent.comp","source.country","source.state","source.city","dest.country",
                "dest.state","admin.region","dest.city","ind.sector","subsector","cluster","ind.activity","value",
                "estimatedv","jobs","estimatedj","project.type")

rest <- read.csv("chinaus1921.csv",stringsAsFactors = FALSE)

restm <- rest %>% 
  dplyr::select(-Free.zone,-Relocation,-Project.status,-Motive.description)

names(restm) <- c("Date","investing.comp","parent.comp","source.country","source.state","source.city","dest.country",
                  "dest.state","admin.region","dest.city","ind.sector","subsector","cluster","ind.activity","jobs",
                  "estimatedj","value","estimatedv","project.type")


fdi <- rbind(fdi,restm)

fdi2 <- fdi %>% 
  mutate(Year=year(Date)) 

# Get values to 2010 $ - Inflation-adjusted value 

getSymbols("CPIAUCSL", src='FRED')

monthly_cpi <- tidy(CPIAUCSL)

yearly_cpi <- monthly_cpi %>% 
  mutate(cpi_year=year(monthly_cpi$index)) %>% 
  group_by(cpi_year) %>% 
  dplyr::summarise(cpi = mean(value))

yearly_cpi$adj_factor10 <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2010]

yearly_cpi <- yearly_cpi %>% 
  rename("Year"=cpi_year) %>% 
  dplyr::select(Year,adj_factor10)

fdi2 <- left_join(fdi2,yearly_cpi)

fdi2 <- fdi2 %>% 
  mutate(value_2010 = value*adj_factor10)

### Filter destination = US 
usfdi <- fdi2 %>% 
  filter(dest.country=="United States") %>% 
  filter(dest.state!="Not Specified" & admin.region!="Not Specified") %>%
  filter(dest.state!="Northern Mariana Islands" & dest.state!="Not Specified")

# Correct and merge county names with FIPS 
usfdi2 <- usfdi %>% 
  separate(admin.region, c("County", "nm"), "[()]")

usfdi3 <- correct_adminregion(usfdi2)

statecross <- read.csv("stateabb.csv",sep=";",stringsAsFactors = FALSE)

statecross <- statecross %>% 
  rename(dest.state=state) %>% 
  mutate(dest.state=ifelse(dest.state=="District of Columbia","Washington, DC",dest.state))

usfdi3$County <- as.character(usfdi3$County)

usfdi3 <- left_join(usfdi3, statecross)

usfdi3$County <- trimws(usfdi3$County)
usfdi3$State <- trimws(usfdi3$State)

# Get county FIPS codes 
county <- read.csv("finalRvote.csv")
county$County <- gsub("[.]","",county$County)

countym <- county %>% 
  filter(Year==2010) %>% 
  dplyr::select(County,State,FIPS) %>%
  distinct()

countym$County <- trimws(countym$County)

usfdifips <- left_join(usfdi3,countym)

# 12 FIPS are missing because of contradictory FIPS coding 
usfdifips <- usfdifips %>% 
  mutate(FIPS=ifelse(County=="Aleutians West Borough"&State!="NJ",2016,
                     ifelse(County=="Anchorage Borough",2020,
                            ifelse(County=="Fairbanks North Star Borough",2090,
                                   ifelse(County=="North Slope Borough",2185,
                                          ifelse(County=="Southeast Fairbanks Borough",2240,
                                                 ifelse(County=="Orleans Parish"&State=="NY",36073,
                                                        ifelse(County=="Orleans Parish"&State=="VT",50019,
                                                               ifelse(County=="Washington, DC",11001,FIPS)))))))))




save(usfdifips, file="usfdi.Rdata")

## Creating the China sample 
# if(ind=="Manufacturing"){ 
#   usfdifips <- usfdifips %>% 
#     filter(ind.activity=="Manufacturing")
# } 

# Include all announcements till September 2012 (included)
# There are 151 China FDI - jobs: 110 is estimated 41 is not value: 126 estimated 25 not 
chinafdi <- usfdifips %>%
  filter(source.country=="China") %>%  
  filter(Date<"2012-09-01") %>% 
  arrange(Date) %>% 
  group_by(State,FIPS) %>% 
  summarise(chinano=n(),
    chinaval=sum(value_2010,na.rm=TRUE),
         chinajobs=sum(jobs,na.rm=TRUE),
    # exclude estimated figures
    chinajobs_noest=sum(jobs[estimatedj=="No"],na.rm=T),
    chinaval_noest=sum(value_2010[estimatedv=="No"],na.rm=T)) %>% 
  mutate(year=2012)

# Include all announcements between October 2012 and September 2020 (excluded for now)
# There are 497 China FDI - jobs: 317 is estimated 180 is not, value: 132 estimated 365 not 
chinafdi20 <- usfdifips %>%
  filter(source.country=="China") %>% 
  mutate(Date=as.Date(Date)) %>% 
  #filter(Date<"2020-09-01") %>% 
  filter(Date>"2011-12-31"&Date<"2020-09-01") %>% 
  arrange(Date) %>% 
  group_by(State,FIPS) %>% 
  summarise(chinano=n(),
    chinaval=sum(value_2010,na.rm=TRUE),
         chinajobs=sum(jobs,na.rm=TRUE),
  # exclude estimated figures
    chinajobs_noest=sum(jobs[estimatedj=="No"],na.rm=T),
    chinaval_noest=sum(value_2010[estimatedv=="No"],na.rm=T)) %>% 
  mutate(year=2020)

# y <- usfdifips %>% 
#   filter(source.country=="China") %>% 
#   mutate(Date=as.Date(Date)) %>% 
#   filter(Date<"2020-09-01") %>% 
#   arrange(Date)
#   count(Year)

chinafditime <- rbind(chinafdi,chinafdi20)

## names of Chinese companies for article scraping

chinaname <- usfdi %>% 
  filter(source.country=="China") %>%
  filter(Date<"2012-09-01") %>%
  dplyr::select(Date,investing.comp,parent.comp,source.country,dest.state,admin.region)

names(chinaname) <- c("date","Investing.Company","Parent.Company","Source.Country","Destination.State",
                      "Admin.Region")

# Delete Inc, Co. and anything in parantheses as well as & because NewsBank can't read them

## Before 2012
chinaname2 <- chinaname %>% 
  mutate(Investing.Company=gsub("\\bCo\\b|\\bCo.\\b|\\bInc.\\b|\\bInc\\b|\\bU.S.\\b|
                                \\bUSA\\b|\\bLimited\\b|\\bLtd\\b|\\bUSA\\b","",Investing.Company),
         Investing.Company=gsub("\\s*\\([^\\)]+\\)","",Investing.Company),
         Investing.Company=gsub("&","and",Investing.Company),
         Investing.Company=gsub("\\bCompany\\b","",Investing.Company),
         Investing.Company=gsub("Bedtime","Bedtime Company",Investing.Company),
         Destination.State=gsub("Washington, DC","District of Columbia",Destination.State),
         Admin.Region=gsub("Washington, DC","Washington (DC)",Admin.Region))

#write.csv(chinaname2,"~/Desktop/Webscrape/chinagfdi/china12comp.csv",row.names=F)


## For 2020 sample 
chinaname20 <- usfdi %>% 
  filter(source.country=="China") %>%
  filter(Date>"2012-09-01"&Date<"2020-09-01") %>%
  dplyr::select(Date,investing.comp,parent.comp,source.country,dest.state,admin.region)

names(chinaname20) <- c("date","Investing.Company","Parent.Company","Source.Country","Destination.State",
                        "Admin.Region")

# Delete Inc, Co. and anything in parantheses as well as & because NewsBank can't read them

chinaname202 <- chinaname20 %>% 
  mutate(Investing.Company=gsub("\\bCo\\b|\\bCo.\\b|\\bInc.\\b|\\bInc\\b|\\bU.S.\\b|
                                \\bUSA\\b|\\bLimited\\b|\\bLtd\\b|\\bUSA\\b","",Investing.Company),
         Investing.Company=gsub("\\s*\\([^\\)]+\\)","",Investing.Company),
         Investing.Company=gsub("&","and",Investing.Company),
         Investing.Company=gsub("\\bCompany\\b","",Investing.Company),
         Investing.Company=gsub("Bedtime","Bedtime Company",Investing.Company),
         Destination.State=gsub("Washington, DC","District of Columbia",Destination.State),
         Admin.Region=gsub("Washington, DC","Washington (DC)",Admin.Region))



#write.csv(chinaname202,"~/Desktop/Webscrape/chinagfdiright/china20comp.csv",row.names=F)


### Create China media measure Merge articles collected with investment - 2012 

artc <- read.csv("comparticles.csv",stringsAsFactors = F)

artc <- artc %>% 
  mutate(compname=trimws(compname))

chinaname3 <- chinaname2 %>% 
  mutate(stab=gsub(".*\\((.*)\\).*", "\\1", Admin.Region),
         compname=paste0(trimws(Investing.Company),"_",stab),
         date=as.character(date),
         compname=ifelse(Investing.Company=="BYD Electronics"&date=="2010-05-01","BYD_CA_2010-05-01",
                         ifelse(Investing.Company=="BYD Electronics"&date=="2010-12-01","BYD_CA_2010-12-01",
                                ifelse(Investing.Company=="Hisense"&date=="2010-07-01","Hisense_GA_2010-07-01",
                                       ifelse(Investing.Company=="Nexteer Automotive"&date=="2012-02-01","Nexteer Automotive_MI_2012-02-01",
                                              ifelse(Investing.Company=="Sany Heavy Industry"&date=="2011-10-01","Sany Heavy Industry_GA_2011-10-01",
                                                     ifelse(Investing.Company=="Suntech Power Holdings"&date=="2010-10-01","Suntech Power Holdings_AZ_2010-10-01",
                                                            ifelse(Investing.Company=="Yingli Green Energy Americas"&date=="2011-07-01","Yingli_CA_2011-07-01",
                                                                   ifelse(Investing.Company=="Huawei Technologies"&date=="2010-10-01","Huawei Technologies_2010-10-01_TX",compname))))))))) %>% 
  left_join(artc) 

saveRDS(chinaname3,file="china12media.Rds")

chinaname3 <- chinaname3 %>% 
  arrange(date) %>% 
  dplyr::select(media)

## Sort dataframes and column bind article numbers 
chinamedia <- usfdifips %>%
  filter(source.country=="China") %>%  
  filter(Date<"2012-09-01") %>%
  arrange(Date)

# column merging is the easiest because I changed company names for NewsBank search 
chinamedia <- cbind(chinamedia,chinaname3)
# 
# f <- chinamedia %>% 
#   filter(media==1)
# 
# write.csv(f,"china12coojobs.csv")

chinamedia <- chinamedia %>% 
  filter(media==1) %>% 
  dplyr::select(-media) %>% 
  group_by(State,FIPS) %>% 
  summarise(mediano=n(),
    mediaval=sum(value_2010,na.rm=TRUE),
         mediajobs=sum(jobs,na.rm=TRUE),
    mediadum=1) %>% 
  mutate(year=2012)

### Create China media measure Merge articles collected with investment - 2020

artc20 <- read.csv("comparticles20.csv",stringsAsFactors = F)

artc20 <- artc20 %>% 
  mutate(compname=trimws(compname))

chinaname32 <- chinaname202 %>% 
  mutate(stab=gsub(".*\\((.*)\\).*", "\\1", Admin.Region),
         compname=trimws(paste0(Investing.Company,"_",stab)),
         date=as.character(format(as.POSIXct(date,format='%Y-%d-%m %H-%M-%S'),format='%Y-%m-%d')),
         compname=ifelse(Investing.Company=="Baidu"&date=="2017-03-01","Baidu_CA_0Mar 2017 - Apr 2017.pdf", 
                         ifelse(Investing.Company=="Baidu"&date=="2014-05-01","Baidu_CA_0May 2014 - Jun 2014.pdf",
                                ifelse(Investing.Company=="Baidu"&date=="2018-10-01","Baidu_CA_0Oct 2018 - Nov 2018.pdf",
                                       ifelse(Investing.Company=="BYD"&date=="2013-04-01","BYD_CA_0Apr 2013 - May 2013.pdf",
                                              ifelse(Investing.Company=="BYD"&date=="2019-04-01","BYD_CA_0Apr 2019 - May 2019.pdf",
                                                     ifelse(Investing.Company=="BYD"&date=="2018-09-01","BYD_CA_0Sep 2018 - Oct 2018.pdf",
                                                            ifelse(Investing.Company=="CW Bearing "&date=="2015-08-01","CW Bearing _MI_0Aug 2015 - Sep 2015.pdf",
                                                                   ifelse(Investing.Company=="Fisker Automotive, "&date=="2015-06-01","Fisker_CA_0Jun 2015 - Jul 2015.pdf",
                                                                          ifelse(Investing.Company=="Fuyao North America"&date=="2015-09-01","Fuyao_OH_0Sep 2015 - Oct 2015.pdf",
                                                                                 ifelse(Investing.Company=="GE Appliances"&date=="2018-06-01","GE Appliances_GA_0Jun 2018 - Jul 2018.pdf",
                                                                                        ifelse(Investing.Company=="GE Appliances"&date=="2019-06-01","GE Appliances_GA_0Jun 2019 - Jul 2019.pdf",
                                                                                               ifelse(Investing.Company=="GE Appliances"&date=="2020-07-01","GE Appliances_KY_0July 2020-August2020.pdf",
                                                                                                      ifelse(Investing.Company=="Ingram Micro"&date=="2019-10-01","Ingram Micro_IN_0Oct 2019 - Nov 2019.pdf",
                                                                                                             ifelse(Investing.Company=="Ingram Micro"&date=="2017-09-01","Ingram Micro_IN_0Sep 2017 - Oct 2017.pdf",
                                                                                                                    ifelse(Investing.Company=="Kiekert AG"&date=="2018-05-01","Kiekert AG_MI_0May 2018 - Jun 2018.pdf",
                                                                                                                           ifelse(Investing.Company=="Kiekert AG"&date=="2013-10-01","Kiekert AG_MI_0Oct 2013 - Nov 2013.pdf",
                                                                                                                                  ifelse(Investing.Company=="Kingfa Science and Technology ()"&date=="2017-02-01","Kingfa Science and Technology ()_MI_0Feb 2017 - Mar 2017.pdf",
                                                                                                                                         ifelse(Investing.Company=="KSM Castings"&date=="2013-02-01","KSM Castings_NC_0Feb 2013 - Mar 2013.pdf",
                                                                                                                                                ifelse(Investing.Company=="KSM Castings"&date=="2016-03-01","KSM Castings_NC_0Mar 2016 - Apr 2016.pdf",
                                                                                                                                                       ifelse(Investing.Company=="Nexteer Automotive"&date=="2015-08-01","Nexteer Automotive_MI_0Aug 2015 - Sep 2015.pdf",
                                                                                                                                                              ifelse(Investing.Company=="Nexteer Automotive"&date=="2017-09-01","Nexteer Automotive_MI_0Sep 2017 - Oct 2017.pdf",
                                                                                                                                                                     ifelse(Investing.Company=="Nexteer Automotive"&date=="2012-12-01","Nexteer Automotive_MI_1Dec 2012 - Jan 2013.pdf",
                                                                                                                                                                            ifelse(Investing.Company=="NIO"&date=="2018-07-01","NIO_CA_0Jul 2018 - Aug 2018.pdf",
                                                                                                                                                                                   ifelse(Investing.Company=="SF Motors"&date=="2018-04-01","SF Motors_CA_0Apr 2018 - May 2018.pdf",
                                                                                                                                                                                          ifelse(Investing.Company=="Suzhou Tianyuan Garments"&date=="2016-10-01","Suzhou Tianyuan Garments_AR_0Oct 2016 - Nov 2016.pdf",
                                                                                                                                                                                                 ifelse(Investing.Company=="Syngenta"&date=="2020-06-01","Syngenta_IL_0Jun 2020 - Jul 2020.pdf",
                                                                                                                                                                                                        ifelse(Investing.Company=="Syngenta"&date=="2018-10-01","Syngenta_IL_0Oct 2018 - Nov 2018.pdf",
                                                                                                                                                                                                               ifelse(Investing.Company=="TikTok"&date=="2020-01-01","TikTok_CA_0Jan 2020 - Feb 2020.pdf",
                                                                                                                                                                                                                      ifelse(Investing.Company=="TuSimple"&date=="2017-08-01","TuSimple_AZ_0Aug 2017 - Sep 2017.pdf",
                                                                                                                                                                                                                             ifelse(Investing.Company=="TuSimple"&date=="2018-09-01","TuSimple_AZ_0Sep 2018 - Oct 2018.pdf",
                                                                                                                                                                                                                                    ifelse(Investing.Company=="Volvo Automotive"&date=="2015-03-01","Volvo_SC_0Mar 2015 - Apr 2015.pdf",
                                                                                                                                                                                                                                           ifelse(Investing.Company=="Fuyao North America"&date=="2014-04-01","Fuyao_MI",
                                                                                                                                                                                                                                                  ifelse(Investing.Company=="GD Copper "&date=="2019-04-01","GD Copper _AL_0April2019-May2019.pdf",
                                                                                                                                                                                                                                                         ifelse(Investing.Company=="Zhongding A.","Zhongding_MI",
                                                                                                                                                                                                                                                                ifelse(Investing.Company=="JD.com","JDcom_CA",
                                                                                                                                                                                                                                                                       ifelse(Investing.Company=="Jiecang Linear Motion Technology"&date=="2017-08-01","Jiecang Linear Motion Technology_MI_0August2017-September2017.pdf",
                                                                                                                                                                                                                                                                              ifelse(Investing.Company=="LeEco"&date=="2016-04-01","LeEco_CA_0April2016-June2016.pdf",
                                                                                                                                                                                                                                                                                     ifelse(Investing.Company=="XIN Development Group International"&date=="2016-08-01","Xinyuan_NY",compname
                                                                                                                                                                                                                                                                                     ))))))))))))))))))))))))))))))))))))))) %>% 
  
  left_join(artc20) 

saveRDS(chinaname32,file="china20media.Rds")

chinaname32 <- chinaname32 %>% 
  arrange(date) %>% 
  dplyr::select(media) 


chinamedia20 <- usfdifips %>%
  filter(source.country==origin) %>%  
  filter(Date>"2012-09-01"&Date<"2020-09-01") %>%
  arrange(Date)

# 2020 - 497 investments, 167 has news reports 
# 
chinamedia20 <- cbind(chinamedia20,chinaname32)

# f <- chinamedia20 %>% 
#   filter(media==1)
# 
# write.csv(f,"china20coojobs.csv")

chinamedia20 <- chinamedia20 %>% 
  filter(media==1) %>% 
  dplyr::select(-media) %>% 
  group_by(State,FIPS) %>% 
  summarise(mediano=n(),
    mediaval=sum(value_2010,na.rm=TRUE),
         mediajobs=sum(jobs,na.rm=TRUE),
    mediadum=1) %>% 
  mutate(year=2020)

chinamediatime <- rbind(chinamedia,chinamedia20)

## Create controls - all other GFDI + placebo - FDI with zero jobs

# 2012
allotherfdi <- usfdifips %>%
  filter(source.country!="China") %>%  
  filter(Date<"2012-09-01") %>% 
  arrange(Date) %>% 
  group_by(State,FIPS) %>% 
  summarise(otherno=n(),
    otherval=sum(value_2010,na.rm=TRUE),
         otherjobs=sum(jobs,na.rm=TRUE)) %>% 
  dplyr::select(State,FIPS,starts_with("other")) %>%
  mutate(year=2012)


zerofdi<- usfdifips %>%
  filter(source.country=="China"&jobs==0) %>%  
  filter(Date<"2012-09-01") %>% 
  arrange(Date) %>% 
  group_by(State,FIPS) %>% 
  summarise(zerono=n(),
    zeroval=sum(value_2010,na.rm=TRUE)) %>% 
  dplyr::select(State,FIPS,starts_with("zero")) %>%
  mutate(year=2012)

# 2020

allotherfdi20 <- usfdifips %>%
  filter(source.country!="China") %>% 
  mutate(Date=as.Date(Date)) %>% 
  #filter(Date<"2020-09-01") %>%
  filter(Date>"2012-09-01"&Date<"2020-09-01") %>%
  arrange(Date) %>% 
  group_by(State,FIPS) %>% 
  summarise(otherno=n(),
    otherval=sum(value_2010,na.rm=TRUE),
         otherjobs=sum(jobs,na.rm=TRUE)) %>%
  mutate(year=2020)

zerofdi20 <- usfdifips %>%
  filter(source.country=="China"&jobs==0) %>% 
  #filter(Date<"2020-09-01") %>%
  filter(Date>"2012-09-01"&Date<"2020-09-01") %>%
  arrange(Date) %>% 
  group_by(State,FIPS) %>% 
  summarise(zerono=n(),
            zeroval=sum(value_2010,na.rm=TRUE)) %>% 
  mutate(year=2020)

allotherfditime <- rbind(allotherfdi,allotherfdi20)  
zerofditime <- rbind(zerofdi,zerofdi20)

## Mergers and Acquisitions 
#2012
chinama <- read.csv("china20002012.csv",skip=1,na.strings=c("","NA"),stringsAsFactors = FALSE)

chinama <- chinama %>% 
  dplyr::select(-X)

chinama2 <- read.csv("china1321.csv",skip=1,na.strings=c("","NA"),stringsAsFactors = FALSE)

chinama <- rbind(chinama,chinama2)

names(chinama) <- c("dateann","dateeff","target.name","target.sic","target.ind","target.state","acq.name",
                    "acq.ind","acq.sic","acq.state","acq.nation","shares.acq","shares.sought","value","city","zip")

chinama$dateann2 <- mdy(chinama$dateann)

chinama$zip <- gsub("CA 94-806","94806",chinama$zip)
chinama$zip <- gsub("FL 33-407","33407",chinama$zip)
chinama$zip <- gsub("MI 48-326","48326",chinama$zip)
chinama$zip <- gsub("NV 89-102","89102",chinama$zip)

write.csv(chinama,"chinama.csv",row.names=F)

chinamaan <- chinama %>% 
  filter(dateann2<"2012-09-01"&dateann2>"2009-12-31") %>% 
  separate(zip,c("zip","na"),sep="-")

chinamazip <- chinamaan %>% 
  filter(!is.na(zip)) %>% 
  dplyr::select(dateann,dateeff,target.name,target.sic,acq.name,acq.sic,value,target.state,zip) %>% 
  mutate(zip=as.numeric(zip))

# Merge zips with counties 
zipcross <- read.csv("zipcountycross.csv")

zipcross2 <- zipcross %>% 
  group_by(ZIP) %>%
  filter(TOT_RATIO == max(TOT_RATIO)) %>% 
  dplyr::select(ZIP,COUNTY) %>% 
  rename(zip=ZIP,
         FIPS=COUNTY)

chinamazip2 <- chinamazip %>% 
  left_join(zipcross2) %>% 
  group_by(FIPS) %>% 
  count() %>% 
  rename(mano=n) %>%
  mutate(year=2012) %>%
  filter(!is.na(FIPS))

## year 2020 
chinamaan2 <- chinama %>% 
  filter(dateann2<"2020-09-01"&dateann2>"2017-12-31")%>% 
  separate(zip,c("zip","na"),sep="-")

chinamazip3 <- chinamaan2 %>% 
  filter(!is.na(zip)) %>% 
  dplyr::select(dateann,dateeff,target.name,target.sic,acq.name,acq.sic,value,target.state,zip) %>% 
  mutate(zip=as.numeric(zip))

chinamazip3 <- chinamazip3 %>% 
  left_join(zipcross2) %>% 
  mutate(FIPS=ifelse(zip==44334,39153,
                     ifelse(zip==27331,37105,
                            ifelse(zip==5131, 95131,
                                   ifelse(zip==40550,1057,FIPS))))) %>% 
  group_by(FIPS) %>% 
  count() %>% 
  rename(mano=n) %>%
  mutate(year=2020) %>% 
  filter(!is.na(FIPS))

chinamatime <- rbind(chinamazip2,chinamazip3)


statecrossm <- statecross %>% 
  rename(target.state=dest.state)

## Create unemployment control from county unemployment - data taken from LAUS - 
# control for unemployment at initial year - exclusion restriction for instrument 2009, 2017 
# control for unemployment change during the period of receiving FDI 2010-2012, 2018-2020 
# control for unemployment for different time periods - very initial 2003, 2011 

# 2003 

unemp03 <- read_unemp("laucnty03.txt",2003)
unemp09 <- read_unemp("laucnty09.txt",2009)
unemp10 <- read_unemp("laucnty10.txt",2010)
unemp11 <- read_unemp("laucnty11.txt",2011)
unemp12 <- read_unemp("laucnty12.txt",2012)
unemp17<- read_unemp("laucnty17.txt",2017)
unemp18 <- read_unemp("laucnty18.txt",2018)
unemp20 <- read_unemp("laucnty20.txt",2020)
## Convert from countries into cds - GFDI, MA, unemployment - using 2010 census county-CD matches 

countycd <- read.csv("countytocd.csv",skip=1)

names(countycd) <- c("FIPS","statefips","cd","stateabb","countyname","pop","afact")

countycdm <- countycd %>% 
  dplyr::select(FIPS,stateabb,cd,afact)

cty12 <- countycdm %>% 
  mutate(year=2012)
cty20 <- countycdm %>% 
  mutate(year=2020)

countycdtime <- rbind(cty12,cty20)

countycdtime2 <- countycdtime %>% 
  left_join(chinafditime) %>% 
  dplyr::select(-State) %>% 
  left_join(.,allotherfditime) %>% 
    dplyr::select(-State) %>%
  left_join(.,chinamediatime) %>% 
  dplyr::select(-State) %>% 
  left_join(.,zerofditime) %>% 
  dplyr::select(-State) %>% 
  left_join(.,chinamatime) %>% 
  left_join(.,unemp03) %>%
  left_join(.,unemp09) %>% 
  left_join(.,unemp10) %>% 
  left_join(.,unemp11) %>% 
  left_join(.,unemp12) %>% 
  left_join(.,unemp17) %>% 
  left_join(.,unemp18) %>% 
  left_join(.,unemp20) %>% 
  mutate(across(chinano:mano,
         ~ifelse(is.na(.x), 0, .x)))

# District-level: Chinese FDI, MA, Other FDI, Zero FDI, Media, Unemployment
# 2707 counties match one-to-one to CDs
countycdtime3 <- countycdtime2 %>% 
  mutate_at(vars(ends_with("no"),ends_with("val"),ends_with("jobs"),ends_with("noest"),
                 starts_with("labforce"),starts_with("unemp")), list(d = ~.*afact)) %>% 
  mutate(cd=ifelse(stateabb=="AK"|stateabb=="DE"|stateabb=="MT"|stateabb=="ND"|stateabb=="SD"|
                     stateabb=="VT"|stateabb=="WY"|stateabb=="DC",1,cd)) %>% 
  mutate(cdids=paste0(stateabb,cd)) %>% 
  dplyr::select(year,cdids,ends_with("_d")) %>% 
  group_by(year,cdids) %>% 
  summarise_all(sum,na.rm=T) %>% 
  mutate(# create initial year unemp rate contrls: 2009, 2017
    unemploy_ivbase_d=ifelse(year==2012,unemp_2009_d/labforce_2009_d,unemp_2017_d/labforce_2017_d),
    # unemployment rate change during FDI years, 2012-2010,2020-2018
    unemploy_diff_d=ifelse(year==2012,(unemp_2012_d/labforce_2012_d)-(unemp_2010_d/labforce_2010_d),
                           (unemp_2020_d/labforce_2020_d)-(unemp_2018_d/labforce_2018_d)),
    # unemployment rate initial for longer time periods, 2003, 2011
    unemploy_initbase_d=ifelse(year==2012, unemp_2003_d/labforce_2003_d,unemp_2011_d/labforce_2011_d)) %>% 
    dplyr::select(-starts_with("unemp_"),-starts_with("labforce_")) 

########### Get controls from Census 

# Prepare census controls for IV base - 2009, 2017 
# Prepare census controls for initial base - 2009, 2012 - because of redistricting and data availability
# Prepare chinese population change between 2010-2012, 2018-2020 

#totalpop, medianinc, chinesepop,malebach,malemaster,maleprof,maledoc,
# femalebach,femalemaster,femaleprof,femaledoc,popforbachelor (25+ pop) - 2009
datacensus09 <- getCensus(name="acs/acs5", 
                        vintage=2009, key=mycensuskey,
                        vars=c("NAME","B01003_001E","B19013_001E","B05006_049E",
                               "B15002_015E","B15002_016E","B15002_017E","B15002_018E",
                               "B15002_032E","B15002_033E","B15002_034E","B15002_035E","B15002_001E"),
                        region="congressional district:*")

datacensus09 <- datacensus09 %>% 
  mutate(bach_ivbase_cd=B15002_015E+B15002_016E+B15002_017E+B15002_018E+B15002_032E+B15002_033E+B15002_034E+B15002_035E,
         popforbach_ivbase_cd=B15002_001E) %>% 
  rename(pop_ivbase_cd=B01003_001E,
         medinc_ivbase_cd=B19013_001E,
         chinesepop_ivbase_cd=B05006_049E,
         name=NAME,
         cd_prev=congressional_district) %>% 
  mutate(year=2012) %>% 
  dplyr::select("state","cd_prev","name","pop_ivbase_cd","medinc_ivbase_cd","chinesepop_ivbase_cd","bach_ivbase_cd","popforbach_ivbase_cd","year")

# totalpop, medianinc, chinesepop, bachelors, grad, popforbachelor - 2012 - 
# census from 2011 onwards has a condensed bachelor category - popforbach is 25+ pop 


datacensus12 <- getCensus(name="acs/acs5", 
                        vintage=2012, key=mycensuskey,
                        vars=c("NAME","B01003_001E","B19019_001E","B05006_048E",
                               "B06009_005E","B06009_006E","B06009_001E"),
                        region="congressional district:*")

datacensus12 <- datacensus12 %>%
  mutate(bach_ivbase_cd=B06009_005E+B06009_006E,
         popforbach_ivbase_cd=B06009_001E) %>% 
  dplyr::select(-starts_with("B06009")) %>%
  mutate(year=2012)


# get census variables for time series - 2017
#totalpop, medianinc, chinesepop, bachelors, grad, popforbachelor
datacensus17 <- getCensus(name="acs/acs5", 
                          vintage=2017, key=mycensuskey,
                          vars=c("NAME","B01003_001E","B19019_001E","B05006_049E",
                                 "B06009_005E","B06009_006E","B06009_001E"),
                          region="congressional district:*")

datacensus17 <- datacensus17 %>%
  mutate(bach_ivbase_cd=B06009_005E+B06009_006E,
         popforbach_ivbase_cd=B06009_001E) %>% 
  dplyr::select(-starts_with("B06009")) %>% 
  mutate(year=2020)

names(datacensus12) <- c("state","cd_prev","name","pop_ivbase_cd","medinc_ivbase_cd","chinesepop_ivbase_cd",
                         "bach_ivbase_cd","popforbach_ivbase_cd","year")

names(datacensus17) <- c("state","cd_prev","name","pop_ivbase_cd","medinc_ivbase_cd","chinesepop_ivbase_cd",
                         "bach_ivbase_cd","popforbach_ivbase_cd","year")

# For 2009,2017
datacensustime_ivbase <- rbind(datacensus09,datacensus17)

datacensus12 <- datacensus12 %>% 
  mutate(year=2020)

# for 2009, 2012 (I'm doing 2009 because ACS goes back till 2009, I'm doing 2012 to match CDs for 2020)
datacensustime_initbase <- rbind(datacensus09,datacensus12)

stcross <- read.csv("statecross.csv",stringsAsFactors = FALSE)

stcross2 <- stcross %>% 
  dplyr::select(statefips,stateab) 

datacensustime_ivbase_m <- datacensustime_ivbase %>% 
  mutate(statefips=as.numeric(state)) %>% 
  left_join(.,stcross2)%>% 
  mutate(stateab=ifelse(statefips==11,"DC",stateab)) %>% 
  # get rid of the residential at large observations 
  filter(!is.na(stateab)) %>% 
  mutate(cd_prev=as.numeric(cd_prev),
         cd_prev=ifelse(stateab=="AK"|stateab=="DE"|stateab=="MT"|stateab=="ND"
                        |stateab=="SD"|stateab=="VT"|stateab=="WY"|stateab=="DC",1,
                        cd_prev), 
         cdids_prev=paste0(stateab,cd_prev)) %>% 
  filter(!is.na(cd_prev)) %>% 
  mutate(perchinese_ivbase_d=chinesepop_ivbase_cd/pop_ivbase_cd,
         percollege_ivbase_d=bach_ivbase_cd/popforbach_ivbase_cd)  %>% 
  rename(pop_ivbase_d=pop_ivbase_cd,
         medinc_ivbase_d=medinc_ivbase_cd) %>% 
  dplyr::select(year,cdids_prev,ends_with("_d"))

datacensustime_initbase_m <- datacensustime_initbase %>% 
  mutate(statefips=as.numeric(state)) %>% 
  left_join(.,stcross2)%>% 
  mutate(stateab=ifelse(statefips==11,"DC",stateab)) %>% 
  filter(!is.na(stateab)) %>% 
  mutate(cd_prev=as.numeric(cd_prev),
         cd_prev=ifelse(stateab=="AK"|stateab=="DE"|stateab=="MT"|stateab=="ND"
                        |stateab=="SD"|stateab=="VT"|stateab=="WY"|stateab=="DC",1,
                        cd_prev), 
         cdids_prev=paste0(stateab,cd_prev)) %>% 
  filter(!is.na(cd_prev)) %>% 
  mutate(perchinese_initbase_d=chinesepop_ivbase_cd/pop_ivbase_cd,
         percollege_initbase_d=bach_ivbase_cd/popforbach_ivbase_cd)  %>% 
  rename(pop_initbase_d=pop_ivbase_cd,
         medinc_initbase_d=medinc_ivbase_cd) %>% 
  dplyr::select(year,cdids_prev,ends_with("_d"))

# totpop, chinesepop to calculate chinese pop change during FDI - 2010 - county bcs redistricting 
chinacensus10 <- getCensus(name="acs/acs5", 
                           vintage=2010, key=mycensuskey,
                           vars=c("NAME","B01003_001E","B05006_049E"),
                           region="congressional district:*")

chinacensus11 <- getCensus(name="acs/acs5", 
                           vintage=2011, key=mycensuskey,
                           vars=c("NAME","B01003_001E","B05006_049E"),
                           region="congressional district:*")

chinacensus11 <- chinacensus11 %>% 
  rename(totpop11=B01003_001E,
         chinapop11=B05006_049E) %>% 
  dplyr::select(state,congressional_district,totpop11,chinapop11)

chinacensus1011 <- chinacensus10 %>% 
  left_join(.,chinacensus11) %>% 
  mutate(statefips=as.numeric(state)) %>% 
  left_join(.,stcross2)%>% 
  mutate(stateab=ifelse(statefips==11,"DC",stateab)) %>% 
  filter(!is.na(stateab)) %>% 
  rename(cd_prev=congressional_district) %>% 
  mutate(cd_prev=as.numeric(cd_prev),
         cd_prev=ifelse(stateab=="AK"|stateab=="DE"|stateab=="MT"|stateab=="ND"
                        |stateab=="SD"|stateab=="VT"|stateab=="WY"|stateab=="DC",1,
                        cd_prev), 
         cdids_prev=paste0(stateab,cd_prev)) %>% 
  filter(!is.na(cd_prev)) %>% 
  mutate(chinesepop_diff_d=(chinapop11/totpop11)-(B05006_049E/B01003_001E)) %>% 
  dplyr::select(cdids_prev,chinesepop_diff_d) %>%
  mutate(year=2012)
  

# totpop, chinesepop to calculate chinese pop change during FDI - 2018
chinacensus18 <- getCensus(name="acs/acs5", 
                           vintage=2018, key=mycensuskey,
                           vars=c("NAME","B01003_001E","B05006_049E"),
                           region="congressional district:*")


# totpop, chinesepop to calculate chinese pop change during FDI - 2020 
chinacensus20 <- getCensus(name="acs/acs5", 
                           vintage=2020, key=mycensuskey,
                           vars=c("NAME","B01003_001E","B05006_049E"),
                           region="congressional district:*")

chinacensus20 <- chinacensus20 %>% 
  rename(totpop20=B01003_001E,
         chinapop20=B05006_049E) %>% 
  dplyr::select(state,congressional_district,totpop20,chinapop20)

chinacensus1820 <- chinacensus18 %>% 
  left_join(.,chinacensus20) %>% 
  mutate(statefips=as.numeric(state)) %>% 
  left_join(.,stcross2)%>% 
  mutate(stateab=ifelse(statefips==11,"DC",stateab)) %>% 
  filter(!is.na(stateab)) %>% 
  rename(cd_prev=congressional_district) %>% 
  mutate(cd_prev=as.numeric(cd_prev),
         cd_prev=ifelse(stateab=="AK"|stateab=="DE"|stateab=="MT"|stateab=="ND"
                        |stateab=="SD"|stateab=="VT"|stateab=="WY"|stateab=="DC",1,
                        cd_prev), 
         cdids_prev=paste0(stateab,cd_prev)) %>% 
  filter(!is.na(cd_prev)) %>% 
  mutate(chinesepop_diff_d=(chinapop20/totpop20)-(B05006_049E/B01003_001E)) %>% 
  dplyr::select(cdids_prev,chinesepop_diff_d) %>%
  mutate(year=2020)

chinesediff <- rbind(chinacensus1011,chinacensus1820)

## Urban, rural districts 
urban10 <- getCensus(name="dec/cd113",vintage=2010,
                   key=mycensuskey,
                   vars=c("NAME","H002001","H002002","H002005"),
                   region="congressional district:*")


urban <- urban10 %>% 
  mutate(statefips=as.numeric(state)) %>% 
  left_join(.,stcross2)%>% 
  mutate(stateab=ifelse(statefips==11,"DC",stateab)) %>% 
  filter(!is.na(stateab)) %>% 
  rename(cd=congressional_district) %>% 
  mutate(cd=as.numeric(cd),
         cd=ifelse(stateab=="AK"|stateab=="DE"|stateab=="MT"|stateab=="ND"
                        |stateab=="SD"|stateab=="VT"|stateab=="WY"|stateab=="DC",1,
                        cd), 
         cdids=paste0(stateab,cd)) %>% 
  filter(!is.na(cd)) %>% 
  mutate(per_urban=(H002002/H002001)*100) %>%
  dplyr::select(cdids,per_urban)
  

##### Get Manufacturing employment 

# Until 2013, CBP reported employment for counties 

# 2003 - 2009 -2011 
create_manucty <- function(filename,year){
  c <- read.table(filename,sep=",",quote="\"'",header=T,na.strings=c("","NA"),stringsAsFactors = FALSE)
  
  c_2 <- c %>% 
    filter(naics=="31----") %>% 
    # Estimate suppressed employment from establishment number
    mutate(emp_est=ifelse(!is.na(empflag),n1_4*2.5 +
                            n5_9*7+
                            n10_19*14.5 + 
                            n20_49 * 34.5 + 
                            n50_99 *74.5 + 
                            n100_249 * 174.5 + 
                            n250_499 * 374.5 +
                            n500_999 * 749.5 +
                            n1000_1 * 1249.5 +
                            n1000_2 * 1999.5 +
                            n1000_3 * 3749.5 +
                            n1000_4 * 6250,NA),
           emp_imputed=ifelse(is.na(emp_est),emp,emp_est),
           # create fips code
           fipscty=sprintf("%03d",as.numeric(fipscty)),
           FIPS=paste0(fipstate,fipscty),
           FIPS=as.numeric(ifelse(FIPS==11999,11001,FIPS))) %>% 
    dplyr::select(FIPS,emp_imputed) %>% 
    rename("emp_imputed_{{year}}" := emp_imputed)
  
  return(c_2)

}

manu03 <- create_manucty("cbp03co.txt",2003)
manu09 <- create_manucty("cbp09co.txt",2009)
manu11 <- create_manucty("cbp11co.txt",2011)

labforce03 <- unemp03 %>% 
  dplyr::select(FIPS,labforce_2003)

labforce09 <- unemp09 %>% 
  dplyr::select(FIPS,labforce_2009)

labforce11 <- unemp11 %>% 
  dplyr::select(FIPS,labforce_2011)

countycdm_manu <- countycdm %>% 
  left_join(.,manu03) %>% 
  left_join(.,manu09) %>% 
  left_join(.,manu11) %>% 
  mutate(across(emp_imputed_2003:emp_imputed_2011,
                ~ifelse(is.na(.x), 0, .x))) %>% 
  left_join(.,labforce03) %>% 
  left_join(.,labforce09) %>% 
  left_join(.,labforce11) %>% 
  mutate(across(c(emp_imputed_2003:labforce_2011), list(d = ~.*afact))) %>% 
  mutate(cd=ifelse(stateabb=="AK"|stateabb=="DE"|stateabb=="MT"|stateabb=="ND"|stateabb=="SD"|
                     stateabb=="VT"|stateabb=="WY"|stateabb=="DC",1,cd)) %>% 
  mutate(cdids=paste0(stateabb,cd)) %>% 
  dplyr::select(cdids,ends_with("_d")) %>% 
  group_by(cdids) %>% 
  summarise_all(sum,na.rm=T) %>% 
  mutate(per_manu_d_03=emp_imputed_2003_d/labforce_2003_d,
         per_manu_d_09=emp_imputed_2009_d/labforce_2009_d,
         per_manu_d_11=emp_imputed_2011_d/labforce_2011_d) %>% 
  dplyr::select(cdids,starts_with("per_manu"))

## 2017 
manu17 <- read.csv("cbp17cd.csv",na.strings=c("","NA"),stringsAsFactors = FALSE)


statecross_v <- statecrossm %>% 
  rename(stateabb=State,
    State=target.state) %>%
  mutate(State=ifelse(State=="Washington, DC","District of Columbia",State))

manu17_2 <- manu17  %>% 
  filter(X2017.NAICS.Code=="31----"|X2017.NAICS.Code=="------") %>% 
  left_join(.,statecross_v) %>%
  filter(!is.na(stateabb)) %>% 
  # create cdids
  mutate(cd=ifelse(stateabb=="AK"|stateabb=="DE"|stateabb=="MT"|stateabb=="ND"|stateabb=="SD"|
                        stateabb=="VT"|stateabb=="WY"|stateabb=="DC",1,X116th.Congressional.District),
    cdids=paste0(stateabb,cd),
    NAICS.Description=ifelse(NAICS.Description=="Total for all sectors","total","manu")) %>% 
  dplyr::select(cdids,NAICS.Description,Employment) %>% 
  pivot_wider(names_from="NAICS.Description",
              values_from=Employment) %>%
  mutate(per_manu_d=as.numeric(manu)/as.numeric(total)) %>%
  dplyr::select(cdids,per_manu_d) %>% 
  mutate(year=2020)

# create merge dataframes 
m09 <- countycdm_manu %>% 
  dplyr::select(cdids,per_manu_d_09) %>% 
  rename(per_manu_d=per_manu_d_09) %>% 
  mutate(year=2012)

manu_time_ivbase <-  rbind(manu17_2,m09)
manu_time_ivbase <- manu_time_ivbase %>% 
  rename(per_manu_ivbase_d=per_manu_d)

manu_time_initbase <- countycdm_manu %>% 
  dplyr::select(-per_manu_d_09) %>% 
  pivot_longer(cols=per_manu_d_03:per_manu_d_11,
               names_to="year",
               values_to="per_manu_d") %>%
  mutate(year=ifelse(year=="per_manu_d_03",2012,2020)) %>%
  rename(per_manu_initbase_d=per_manu_d)
  
##### Get district ideology - previous 2 presidential elections 

# for 2012 
pres <- read.csv("ERDLE10A.csv",stringsAsFactors = FALSE)

presideo <- pres %>% 
  mutate(ED=ifelse(ED==99,1,ED),
         cdids_prev=paste0(STA,ED)) %>% 
  dplyr::select(cdids_prev,G08P_RP,G04P_RP) %>% 
  rename(presvote_lag2=G04P_RP,
         presvote= G08P_RP) %>% 
  mutate(year=2012)


# 2020 

pres20 <- read.csv("erdQI14B.csv",stringsAsFactors = FALSE)

presideo20 <- pres20 %>% 
  mutate(ED=ifelse(ED==99,1,ED),
         cdids_prev=paste0(STA,ED)) %>% 
  dplyr::select(cdids_prev,G16P_RP,G12P_RP) %>% 
  rename(presvote_lag2=G12P_RP,
         presvote=G16P_RP) %>% 
  mutate(year=2020)

prestime <- rbind(presideo,presideo20)

## Add China trade shock variable -2000-2007 
# Shock is measured at the commuting zone level 
# At each district, shock will be the population-share-weighted average of the shock in commuting zones 
# It is like going from commuting zone to county and then matching with district
# For 2012 file - using 2002 population to allocate trade shocks 
# For 2020 file - using 2010 population to allocate trade shocks 

# for 2012 
adh <- read.dta13("workfile_china.dta") %>% 
  filter(t2==1) %>% 
  dplyr::select(czone,d_tradeusch_pw,d_tradeotch_pw_lag)

adhcd <- read.dta13("house_2002_2016.dta") %>% 
  mutate(cdids_prev=gsub(" ","",congressionaldistrict)) %>%
  dplyr::select(cdids_prev,cty_fips,czone,sh_district_2002) %>%
  arrange(cdids_prev,cty_fips)

adhm <- left_join(adhcd,adh) %>% 
  mutate(tradeshock_d=sh_district_2002*d_tradeusch_pw,
         tradeshocki_d=sh_district_2002*d_tradeotch_pw_lag) %>% 
  group_by(cdids_prev) %>% 
  summarise_at(vars(tradeshock_d,tradeshocki_d),sum,na.rm=T) %>%
  mutate(year=2012)

# for 2020 
cdcounty <- read.csv("geocorr2018_2302400645.csv",stringsAsFactors = F)

cdcountym <- cdcounty %>% 
  slice(-1) %>% 
  mutate(cd116=ifelse(stab=="AK"|stab=="DE"|stab=="MT"|stab=="ND"|stab=="SD"|
                        stab=="VT"|stab=="WY"|stab=="DC","01",cd116),
         cdids_prev=paste0(stab,as.numeric(cd116)),
         cty_fips=as.numeric(county)) %>%
  dplyr::select(cdids_prev,cty_fips,afact)

adh_c20 <- adhcd %>% 
  dplyr::select(cty_fips,czone)

adhm_20 <- cdcountym %>% 
  left_join(.,adh_c20) %>%
  distinct() %>% 
  left_join(.,adh) %>% 
  mutate(afact=as.numeric(afact)) %>% 
  mutate(tradeshock_d=afact*d_tradeusch_pw,
         tradeshocki_d=afact*d_tradeotch_pw_lag) %>% 
  group_by(cdids_prev) %>% 
  summarise_at(vars(tradeshock_d,tradeshocki_d),sum,na.rm=T) %>% 
  filter(!(cdids_prev=="AK1"|cdids_prev=="HI1"|cdids_prev=="HI2"|cdids_prev=="DC1")) %>% 
  mutate(year=2020)
  
adh_time <- rbind(adhm,adhm_20)
  



############### Merge everything together 
# AK, HI, DC - some variables are missing 

# fix at-large districts at anestime 

anesdistricttime <- anestime  %>%
  # trade shock data
  left_join(.,adh_time) %>% 
  # Chinese FDI, Chinese M&A, Chinese news articles, Other FDI, Chinese FDI (excl estimates) - 2004-2012, 2012-2020 
  # unemployment (2003,2011 - initial base years/2009,2017,during IV FDI)
  left_join(.,countycdtime3) %>% 
  # Census vars - population, median  income, % college, % chinese - for 2009, 2017 iv base years
  left_join(.,datacensustime_ivbase_m) %>%  
  # Census vars population, median  income, % college, % chinese - for 2009, 2012 initial base years
  left_join(.,datacensustime_initbase_m) %>%
  # Chinese population change during IV FDI time - 2010-2011, 2018-2020
  left_join(.,chinesediff) %>%  
  # % Manufacturing - 2009, 2017 - IV base years 
  left_join(.,manu_time_ivbase) %>% 
  # % Manufacturing - 2003, 2011 - initial base years
  left_join(.,manu_time_initbase) %>% 
  # Presidential elections 
  left_join(.,prestime) %>% 
  mutate(rep_d=ifelse(presvote>50,1,0)) %>%
  left_join(.,urban)

# log population and other Chinese fdi vars 
anesdistricttime <- anesdistricttime %>% 
  mutate_at(vars("chinano_d","chinajobs_d","chinaval_d","mano_d",
                 "otherno_d","otherval_d","otherjobs_d",
                 "mediano_d","mediaval_d","mediajobs_d",
                 "zerono_d","zeroval_d"), list(log = ~logplus(.))) 
  # mutate_at(vars("chinano_d","chinajobs_d","chinaval_d","mano_d",
  #                "otherno_d","otherval_d","otherjobs_d",
  #                "mediano_d","mediaval_d","mediajobs_d",
  #                "zerono_d","zeroval_d"), list(pop = ~ logplus(./pop_d_t)))

saveRDS(anesdistricttime,file="anes1220analysis.Rds")


################### IV 
anesdistricttime <- readRDS("anes1220analysis.Rds")
load("usfdi.Rdata")

# combine media separately constructed for 2012 and 2020 
ch_media12 <- readRDS("china12media.Rds")
ch_media20 <-  readRDS("china20media.Rds")

ch_media_time <- rbind(ch_media12,ch_media20)
ch_media_time <- ch_media_time %>% 
  mutate(Date=as.Date(date)) %>%
  dplyr::select(-date)

# County to district appropriations using 2010 census proportions 
countycd <- read.csv("countytocd.csv",skip=1)

names(countycd) <- c("FIPS","statefips","cd","stateabb","countyname","pop","afact")

countycdm <- countycd %>% 
  dplyr::select(FIPS,stateabb,cd,afact) %>% 
  mutate(cd=ifelse(stateabb=="DC",1,cd))
  

# Fetch 2000 CD population 

# apis <- listCensusApis()

# f <- listCensusMetadata(
#   name = "acs/acs5",
#   vintage=2010,
#   type = "variables")

########## Creating IV for 2010-2012/2018-2020 period
## Creating IV Function 

# "2009-12-31","2012-09-01",2003,2009
# "2017-12-31","2020-09-01",2012,2017

# dateend="2020-09-01"
# datestart = "2017-12-31"
# baseyearend=2017
# baseyearst=2012
# census=datacensus102
# Creates FDI variables for China, control for other FDI in the district 
create_iv_fdi <- function(datestart,dateend,baseyearst,baseyearend){
  # growth in Chinese companies in the US
  nationalchina <- usfdifips %>%
    filter(source.country=="China") %>%
    filter(Date<dateend&Date>datestart)%>% 
    summarise(ch_total=n(),
              ch_totaljobs=sum(jobs,na.rm=T),
              ch_totalval=sum(value_2010,na.rm=T),
              # exclude estimated value and jobs
              ch_totaljobs_noest=sum(jobs[estimatedj=="No"],na.rm=T),
              ch_totalval_noest=sum(value_2010[estimatedv=="No"],na.rm=T))
  
  #growth in Chinese companies at county level
  localchina <- usfdifips %>%
    filter(source.country=="China") %>% 
    mutate(Date=as.Date(Date)) %>% 
    filter(Date<dateend&Date>datestart) %>% 
    arrange(Date)
  
  # extract from the media df the relevant dates 
  # company names ar different in two datasets, that's why I have to do column bind even though it is horrible
  ch_med <- ch_media_time %>% 
    filter(Date<dateend&Date>datestart) %>% 
    arrange(Date) %>% 
    dplyr::select(media)
    
  localchina <- cbind(localchina,ch_med)  
  
  localchina <- localchina %>% 
    group_by(FIPS) %>% 
    summarise(ch_cty=n(),
              ch_ctyjobs=sum(jobs,na.rm=T),
              ch_ctyval=sum(value_2010,na.rm=T),
              ## Media vs no media report
              ch_ctymedia=sum(media==1,na.rm=T),
              ch_ctyjobsmedia=sum(jobs[media==1],na.rm=T),
              ch_ctyvalmedia=sum(value_2010[media==1],na.rm=T),
              ch_ctynomedia=sum(is.na(media),na.rm=T),
              ch_ctyjobsnomedia=sum(jobs[is.na(media)],na.rm=T),
              ch_ctyvalnomedia=sum(value_2010[is.na(media)],na.rm=T),
              # exclude estimated value and jobs 
              ch_ctyjobs_noest=sum(jobs[estimatedj=="No"],na.rm=T),
              ch_ctyval_noest=sum(value_2010[estimatedv=="No"],na.rm=T))
  
  # creating baseline share of chinese companies among foreign companies 
  china <- usfdifips %>% 
    filter(baseyearst<Year&Year<baseyearend)  %>% 
    group_by(FIPS) %>% 
    summarise(# Total FDI base 
      allgfdi_base=n(),
              allgfdijobs_base=sum(jobs,na.rm=T),
              allgfdival_base=sum(value_2010,na.rm=T),
              # exclude estimated value and jobs 
              allgfdijobs_base_noest=sum(jobs[estimatedj=="No"],na.rm=T),
              allgfdival_base_noest=sum(value_2010[estimatedv=="No"],na.rm=T),
              # Base China 
              chinafdi_base=sum(source.country=="China"),
              chinafdijobs_base=sum(jobs[source.country=="China"],na.rm=T),
              chinafdival_base=sum(value_2010[source.country=="China"],na.rm=T),
      # exclude estimated value and jobs
      chinafdijobs_base_noest=sum(jobs[source.country=="China"&estimatedj=="No"],na.rm=T),
      chinafdival_base_noest=sum(value_2010[source.country=="China"&estimatedv=="No"],na.rm=T))
  
  # other FDI in the county in the same time period - create as control 
  otherfdi <- usfdifips %>%
    filter(source.country!="China") %>%  
    filter(Date<dateend&Date>datestart)%>% 
    group_by(FIPS) %>% 
    summarise(othernoiv=n(),
              otherjobsiv=sum(jobs,na.rm=T),
              othervaliv=sum(value_2010,na.rm=T),
              # exclude estimates 
              otherjobsiv_noest=sum(jobs[estimatedj=="No"],na.rm=T),
              othervaliv_noest=sum(value_2010[estimatedv=="No"],na.rm=T))
  
  # merge into a list of counties for district transformation
  countycdmiv <- countycdm %>% 
    left_join(.,localchina) %>% 
    left_join(.,china) %>% 
    left_join(.,otherfdi) %>% 
    mutate(across(c(-FIPS,-stateabb,-cd,-afact), ~ifelse(is.na(.x), 0, .x)))
  
  # code IVs 
  countycdmiv2 <- countycdmiv %>% 
    mutate(across(c(-FIPS,-stateabb,-cd,-afact), list(d = ~.*afact))) %>% 
    mutate(cd=gsub("^0$","1",cd),
           cdids=paste0(stateabb,cd)) %>% 
    dplyr::select(-stateabb,-cd) %>% 
    group_by(cdids) %>% 
    summarise_at(vars(ends_with("d")), sum,na.rm=TRUE) %>% 
    # merge national aggregate values to the df
    mutate(ch_total=nationalchina$ch_total,
           ch_totaljobs=nationalchina$ch_totaljobs,
           ch_totalval=nationalchina$ch_totalval,
           # exclude estimates
           ch_totaljobs_noest=nationalchina$ch_totaljobs_noest,
           ch_totalval_noest=nationalchina$ch_totalval_noest) %>% 
    mutate(# subtract ond CD chinese companies from the national aggregate
      chinaoth=ch_total-ch_cty_d, 
      chinaothjobs=ch_totaljobs-ch_ctyjobs_d,
      chinaothval=ch_totalval-ch_ctyval_d,
      # excluding estimates
      chinaothjobs_noest=ch_totaljobs_noest-ch_ctyjobs_noest_d,
      chinaothval_noest= ch_totalval_noest-ch_ctyval_noest_d,
      # create fraction of chinese companies among all other foreign companies
      frach_base=ifelse(chinafdi_base_d != 0 | allgfdi_base_d != 0, chinafdi_base_d/allgfdi_base_d,0),
      frachjobs_base=ifelse(chinafdijobs_base_d != 0 | allgfdijobs_base_d != 0,chinafdijobs_base_d/allgfdijobs_base_d,0),
      frachval_base=ifelse(chinafdival_base_d != 0 | allgfdival_base_d != 0, chinafdival_base_d/allgfdival_base_d,0),
      # excluding estimates 
      frachjobs_base_noest=ifelse(chinafdijobs_base_noest_d != 0 | allgfdijobs_base_noest_d != 0,chinafdijobs_base_noest_d/allgfdijobs_base_noest_d,0),
      frachval_base_noest=ifelse(chinafdival_base_noest_d != 0 | allgfdival_base_noest_d != 0, chinafdival_base_noest_d/allgfdival_base_noest_d,0),
      # create instrument national * base fraction
      z=chinaoth*frach_base,
      zjobs=chinaothjobs*frachjobs_base,
      zval=chinaothval*frachval_base,
      #excluding estimates 
      zjobs_noest=chinaothjobs_noest*frachjobs_base_noest,
      zval_noest=chinaothval_noest*frachval_base_noest,
      #Independent variables
      chinanoiv_d_log=log(ch_cty_d+0.1),
      chinajobsiv_d_log=log(ch_ctyjobs_d+0.1),
      chinavaliv_d_log=log(ch_ctyval_d+0.1),
      # excluding estimates 
      chinajobsiv_noest_d_log=log(ch_ctyjobs_noest_d+0.1),
      chinavaliv_noest_d_log=log(ch_ctyval_noest_d+0.1),
      # Chinese GFDI independent vars broken down by media and no media 
      medianoiv_d_log=log(ch_ctymedia_d+0.1),
      mediajobsiv_d_log=log(ch_ctyjobsmedia_d+0.1),
      mediavaliv_d_log=log(ch_ctyvalmedia_d+0.1),
      # No media 
      nomedianoiv_d_log=log(ch_ctynomedia_d+0.1),
      nomediajobsiv_d_log=log(ch_ctyjobsnomedia_d+0.1),
      nomediavaliv_d_log=log(ch_ctyvalnomedia_d+0.1),
      # Logging the zs 
      z_log=log(z+0.1),
      zjobs_log=log(zjobs+0.1),
      zval_log=log(zval+0.1),
      zjobs_noest_log=log(zjobs_noest+0.1),
      zval_noest_log=log(zval_noest+0.1),
      # Other FDI vars 
      othernoiv_d_log=log(othernoiv_d+0.1),
      otherjobsiv_d_log=log(otherjobsiv_d+0.1),
      othervaliv_d_log=log(othervaliv_d+0.1),
      otherjobsiv_noest_d_log=log(otherjobsiv_noest_d+0.1),
      othervaliv_noest_d_log=log(othervaliv_noest_d+0.1)) %>%  
    dplyr::select(cdids,starts_with("z"),ends_with("log"),
                  ch_cty_d,ch_ctyjobs_d,ch_ctyval_d,
                  ch_ctymedia_d,ch_ctyjobsmedia_d,ch_ctyvalmedia_d,
                  ch_ctynomedia_d,ch_ctyjobsnomedia_d,ch_ctyvalnomedia_d,
                  othernoiv_d,otherjobsiv_d,othervaliv_d,
                  chinafdi_base_d,allgfdi_base_d) 
  
  return(countycdmiv2)
}

chinesegfdi_d_12 <- create_iv_fdi("2009-12-31","2012-09-01",2004,2009)
chinesegfdi_d_20 <- create_iv_fdi("2017-12-31","2020-09-01",2012,2017)

chinesegfdi_d_12$year <- 2012
chinesegfdi_d_20$year <- 2020

cfditime <- rbind(chinesegfdi_d_12,chinesegfdi_d_20)

anesdistricttime2 <- left_join(anesdistricttime,cfditime)


# i <- usfdifips %>%
#   filter(source.country=="China") %>%
#   filter(Date<"2020-09-01")

# dateend="2020-09-01"
# datestart = "2017-12-31"
# baseyearend=2017
# baseyearst=2012
# census=datacensus102
# ind="Software & IT services"
# name="it"
create_indiv_fdi <- function(datestart,dateend,baseyearst,baseyearend,ind,name){
  # growth in Chinese companies in the US
  nationalchina <- usfdifips %>%
    filter(ind.sector!=ind) %>% 
    filter(source.country=="China") %>%  
    filter(Date<dateend&Date>datestart)%>% 
    summarise(ch_total=n(),
              ch_totaljobs=sum(jobs,na.rm=T),
              ch_totalval=sum(value_2010,na.rm=T))
  
  #growth in Chinese companies at county level
  localchina <- usfdifips %>%
    filter(ind.sector!=ind) %>% 
    filter(source.country=="China") %>% 
    mutate(Date=as.Date(Date)) %>% 
    filter(Date<dateend&Date>datestart) %>% 
    arrange(Date) %>% 
    group_by(FIPS) %>% 
    summarise(ch_cty=n(),
              ch_ctyjobs=sum(jobs,na.rm=T),
              ch_ctyval=sum(value_2010,na.rm=T))
  
  # creating baseline share of chinese companies among foreign companies 
  china <- usfdifips %>% 
    filter(ind.sector!=ind) %>% 
    filter(baseyearst<Year&Year<baseyearend)  %>% 
    group_by(FIPS) %>% 
    summarise(# Total FDI base 
      allgfdi_base=n(),
      allgfdijobs_base=sum(jobs,na.rm=T),
      allgfdival_base=sum(value_2010,na.rm=T),
      # Base China 
      chinafdi_base=sum(source.country=="China"),
      chinafdijobs_base=sum(jobs[source.country=="China"],na.rm=T),
      chinafdival_base=sum(value_2010[source.country=="China"],na.rm=T))
  
  # merge into a list of counties for district transformation
  countycdmiv <- countycdm %>% 
    left_join(.,localchina) %>% 
    left_join(.,china) %>% 
    mutate(across(c(-FIPS,-stateabb,-cd,-afact), ~ifelse(is.na(.x), 0, .x)))
  
  # code IVs 
  countycdmiv2 <- countycdmiv %>% 
    mutate(across(c(-FIPS,-stateabb,-cd,-afact), list(d = ~.*afact))) %>% 
    mutate(cd=gsub("^0$","1",cd),
           cdids=paste0(stateabb,cd)) %>% 
    dplyr::select(-stateabb,-cd) %>% 
    group_by(cdids) %>% 
    summarise_at(vars(ends_with("d")), sum,na.rm=TRUE) %>% 
    # merge national aggregate values to the df
    mutate(ch_total=nationalchina$ch_total,
           ch_totaljobs=nationalchina$ch_totaljobs,
           ch_totalval=nationalchina$ch_totalval) %>% 
    mutate(# subtract ond CD chinese companies from the national aggregate
      chinaoth=ch_total-ch_cty_d, 
      chinaothjobs=ch_totaljobs-ch_ctyjobs_d,
      chinaothval=ch_totalval-ch_ctyval_d,
      # create fraction of chinese companies among all other foreign companies
      frach_base=ifelse(chinafdi_base_d != 0 | allgfdi_base_d != 0, chinafdi_base_d/allgfdi_base_d,0),
      frachjobs_base=ifelse(chinafdijobs_base_d != 0 | allgfdijobs_base_d != 0,chinafdijobs_base_d/allgfdijobs_base_d,0),
      frachval_base=ifelse(chinafdival_base_d != 0 | allgfdival_base_d != 0, chinafdival_base_d/allgfdival_base_d,0),
      # create instrument national * base fraction
      z=chinaoth*frach_base,
      zjobs=chinaothjobs*frachjobs_base,
      zval=chinaothval*frachval_base,
      #Independent variables
      "{name}_chinanoiv_d_log":=log(ch_cty_d+0.1),
      "{name}_chinajobsiv_d_log":=log(ch_ctyjobs_d+0.1),
      "{name}_chinavaliv_d_log":=log(ch_ctyval_d+0.1),
      # Logging the zs 
      "{name}_z_log":=log(z+0.1),
      "{name}_zjobs_log":=log(zjobs+0.1),
      "{name}_zval_log":=log(zval+0.1)) %>%  
    dplyr::select(cdids,ends_with("log")) 
  
  return(countycdmiv2)
}
# Excluding top 4 industry activities in Chinese investment 
itgfdi_d_12 <- create_indiv_fdi("2009-12-31","2012-09-01",2004,2009,"Software & IT services","it")
itgfdi_d_20 <- create_indiv_fdi("2017-12-31","2020-09-01",2012,2017,"Software & IT services","it")

itgfdi_d_12$year <- 2012
itgfdi_d_20$year <- 2020

ittime <- rbind(itgfdi_d_12,itgfdi_d_20)

indgfdi_d_12 <- create_indiv_fdi("2009-12-31","2012-09-01",2004,2009,"Industrial equipment","ind")
indgfdi_d_20 <- create_indiv_fdi("2017-12-31","2020-09-01",2012,2017,"Industrial equipment","ind")

indgfdi_d_12$year <- 2012
indgfdi_d_20$year <- 2020

indtime <- rbind(indgfdi_d_12,indgfdi_d_20)

autogfdi_d_12 <- create_indiv_fdi("2009-12-31","2012-09-01",2004,2009,"Automotive components","auto")
autogfdi_d_20 <- create_indiv_fdi("2017-12-31","2020-09-01",2012,2017,"Automotive components","auto")

autogfdi_d_12$year <- 2012
autogfdi_d_20$year <- 2020

autotime <- rbind(autogfdi_d_12,autogfdi_d_20)

busgfdi_d_12 <- create_indiv_fdi("2009-12-31","2012-09-01",2004,2009,"Business services","bus")
busgfdi_d_20 <- create_indiv_fdi("2017-12-31","2020-09-01",2012,2017,"Business services","bus")

busgfdi_d_12$year <- 2012
busgfdi_d_20$year <- 2020

bustime <- rbind(busgfdi_d_12,busgfdi_d_20)


anesdistricttime2 <- left_join(anesdistricttime2,ittime) %>%
  left_join(.,indtime) %>% 
  left_join(.,autotime) %>% 
  left_join(.,bustime)

## Create Asia IV 

# dateend="2020-09-01"
# datestart = "2017-12-31"
# baseyearend=2017
# baseyearst=2012
# census=datacensus102
# countries = c("Japan","Thailand")
# Creates FDI variables for China, control for other FDI in the district 

create_asiaiv_fdi <- function(datestart,dateend,baseyearst,baseyearend,countries){
  # growth in Chinese companies in the US
  nationalchina <- usfdifips %>%
    filter(source.country %in% countries) %>% 
    filter(Date<dateend&Date>datestart)%>% 
    summarise(ch_total=n(),
              ch_totaljobs=sum(jobs,na.rm=T),
              ch_totalval=sum(value_2010,na.rm=T))
  
  #growth in Chinese companies at county level
  localchina <- usfdifips %>%
    filter(source.country %in% countries) %>% 
    mutate(Date=as.Date(Date)) %>% 
    filter(Date<dateend&Date>datestart) %>% 
    arrange(Date) %>% 
    group_by(FIPS) %>% 
    summarise(ch_cty=n(),
              ch_ctyjobs=sum(jobs,na.rm=T),
              ch_ctyval=sum(value_2010,na.rm=T))
  
  # creating baseline share of chinese companies among foreign companies 
  china <- usfdifips %>% 
    filter(baseyearst<Year&Year<baseyearend)  %>% 
    group_by(FIPS) %>% 
    summarise(# Total FDI base 
      allgfdi_base=n(),
      allgfdijobs_base=sum(jobs,na.rm=T),
      allgfdival_base=sum(value_2010,na.rm=T),
      # Base China 
  chinafdi_base=sum(source.country %in% countries),
  chinafdijobs_base=sum(jobs[source.country %in% countries],na.rm=T),
  chinafdival_base=sum(value_2010[source.country %in% countries],na.rm=T))

  
  # merge into a list of counties for district transformation
  countycdmiv <- countycdm %>% 
    left_join(.,localchina) %>% 
    left_join(.,china) %>% 
    mutate(across(c(-FIPS,-stateabb,-cd,-afact), ~ifelse(is.na(.x), 0, .x)))
  
  # code IVs 
  countycdmiv2 <- countycdmiv %>% 
    mutate(across(c(-FIPS,-stateabb,-cd,-afact), list(d = ~.*afact))) %>% 
    mutate(cd=gsub("^0$","1",cd),
           cdids=paste0(stateabb,cd)) %>% 
    dplyr::select(-stateabb,-cd) %>% 
    group_by(cdids) %>% 
    summarise_at(vars(ends_with("d")), sum,na.rm=TRUE) %>% 
    # merge national aggregate values to the df
    mutate(ch_total=nationalchina$ch_total,
           ch_totaljobs=nationalchina$ch_totaljobs,
           ch_totalval=nationalchina$ch_totalval) %>% 
    mutate(# subtract ond CD chinese companies from the national aggregate
      chinaoth=ch_total-ch_cty_d, 
      chinaothjobs=ch_totaljobs-ch_ctyjobs_d,
      chinaothval=ch_totalval-ch_ctyval_d,
      # create fraction of chinese companies among all other foreign companies
      frach_base=ifelse(chinafdi_base_d != 0 | allgfdi_base_d != 0, chinafdi_base_d/allgfdi_base_d,0),
      frachjobs_base=ifelse(chinafdijobs_base_d != 0 | allgfdijobs_base_d != 0,chinafdijobs_base_d/allgfdijobs_base_d,0),
      frachval_base=ifelse(chinafdival_base_d != 0 | allgfdival_base_d != 0, chinafdival_base_d/allgfdival_base_d,0),
      # create instrument national * base fraction
      z=chinaoth*frach_base,
      zjobs=chinaothjobs*frachjobs_base,
      zval=chinaothval*frachval_base,
      #Independent variables
      asianoiv_d_log=log(ch_cty_d+0.1),
      asiajobsiv_d_log=log(ch_ctyjobs_d+0.1),
      asiavaliv_d_log=log(ch_ctyval_d+0.1),
      # Logging the zs 
      asiaz_log=log(z+0.1),
      asiazjobs_log=log(zjobs+0.1),
      asiazval_log=log(zval+0.1)) %>%  
    dplyr::select(cdids,asianoiv_d_log,asiavaliv_d_log,asiajobsiv_d_log,
                  asiaz_log,asiazval_log,asiazjobs_log) 
  
  return(countycdmiv2)
}

#countries = c("Japan","Mongolia","South Korea","Hong Kong","Taiwan")
countries = c("Taiwan")

asiagfdi_d_12 <- create_asiaiv_fdi("2009-12-31","2012-09-01",2004,2009,countries)
asiagfdi_d_20 <- create_asiaiv_fdi("2017-12-31","2020-09-01",2012,2017,countries)

asiagfdi_d_12$year <- 2012
asiagfdi_d_20$year <- 2020

afditime <- rbind(asiagfdi_d_12,asiagfdi_d_20)

anesdistricttime2 <- left_join(anesdistricttime2,afditime) 

## All FDI other than China 

create_nonchinaiv_fdi <- function(datestart,dateend,baseyearst,baseyearend,countries){
  # growth in Chinese companies in the US
  nationalchina <- usfdifips %>%
    filter(!(source.country %in% countries)) %>%
    filter(Date<dateend&Date>datestart)%>% 
    summarise(ch_total=n(),
              ch_totaljobs=sum(jobs,na.rm=T),
              ch_totalval=sum(value_2010,na.rm=T))
  
  #growth in Chinese companies at county level
  localchina <- usfdifips %>%
    filter(!(source.country %in% countries)) %>%
    mutate(Date=as.Date(Date)) %>% 
    filter(Date<dateend&Date>datestart) %>% 
    arrange(Date) %>% 
    group_by(FIPS) %>% 
    summarise(ch_cty=n(),
              ch_ctyjobs=sum(jobs,na.rm=T),
              ch_ctyval=sum(value_2010,na.rm=T))
  
  # creating baseline share of chinese companies among foreign companies 
  china <- usfdifips %>% 
    filter(baseyearst<Year&Year<baseyearend)  %>% 
    group_by(FIPS) %>% 
    summarise(# Total FDI base 
      allgfdi_base=n(),
      allgfdijobs_base=sum(jobs,na.rm=T),
      allgfdival_base=sum(value_2010,na.rm=T),
      # Base China 
      chinafdi_base=sum(!(source.country %in% countries)),
      chinafdijobs_base=sum(jobs[!(source.country %in% countries)],na.rm=T),
      chinafdival_base=sum(value_2010[!(source.country %in% countries)],na.rm=T))
  
  
  # merge into a list of counties for district transformation
  countycdmiv <- countycdm %>% 
    left_join(.,localchina) %>% 
    left_join(.,china) %>% 
    mutate(across(c(-FIPS,-stateabb,-cd,-afact), ~ifelse(is.na(.x), 0, .x)))
  
  # code IVs 
  countycdmiv2 <- countycdmiv %>% 
    mutate(across(c(-FIPS,-stateabb,-cd,-afact), list(d = ~.*afact))) %>% 
    mutate(cd=gsub("^0$","1",cd),
           cdids=paste0(stateabb,cd)) %>% 
    dplyr::select(-stateabb,-cd) %>% 
    group_by(cdids) %>% 
    summarise_at(vars(ends_with("d")), sum,na.rm=TRUE) %>% 
    # merge national aggregate values to the df
    mutate(ch_total=nationalchina$ch_total,
           ch_totaljobs=nationalchina$ch_totaljobs,
           ch_totalval=nationalchina$ch_totalval) %>% 
    mutate(# subtract ond CD chinese companies from the national aggregate
      chinaoth=ch_total-ch_cty_d, 
      chinaothjobs=ch_totaljobs-ch_ctyjobs_d,
      chinaothval=ch_totalval-ch_ctyval_d,
      # create fraction of chinese companies among all other foreign companies
      frach_base=ifelse(chinafdi_base_d != 0 | allgfdi_base_d != 0, chinafdi_base_d/allgfdi_base_d,0),
      frachjobs_base=ifelse(chinafdijobs_base_d != 0 | allgfdijobs_base_d != 0,chinafdijobs_base_d/allgfdijobs_base_d,0),
      frachval_base=ifelse(chinafdival_base_d != 0 | allgfdival_base_d != 0, chinafdival_base_d/allgfdival_base_d,0),
      # create instrument national * base fraction
      z=chinaoth*frach_base,
      zjobs=chinaothjobs*frachjobs_base,
      zval=chinaothval*frachval_base,
      #Independent variables
      nonchinanoiv_d_log=log(ch_cty_d+0.1),
      nonchinajobsiv_d_log=log(ch_ctyjobs_d+0.1),
      nonchinavaliv_d_log=log(ch_ctyval_d+0.1),
      # Logging the zs 
      nonchinaz_log=log(z+0.1),
      nonchinazjobs_log=log(zjobs+0.1),
      nonchinazval_log=log(zval+0.1)) %>%  
    dplyr::select(cdids,nonchinanoiv_d_log,nonchinavaliv_d_log,nonchinajobsiv_d_log,
                  nonchinaz_log,nonchinazval_log,nonchinazjobs_log) 
  
  return(countycdmiv2)
}

countries2=c("China")

nonchinagfdi_d_12 <- create_nonchinaiv_fdi("2009-12-31","2012-09-01",2004,2009,countries2)
nonchinagfdi_d_20 <- create_nonchinaiv_fdi("2017-12-31","2020-09-01",2012,2017,countries2)

nonchinagfdi_d_12$year <- 2012
nonchinagfdi_d_20$year <- 2020

ncfditime <- rbind(nonchinagfdi_d_12,nonchinagfdi_d_20)

anesdistricttime2 <- left_join(anesdistricttime2,ncfditime) 



## Different timing 

### 8 months 
cgfdi_12_8mo <- create_iv_fdi("2011-12-31","2012-09-01",2004,2009)
cgfdi_20_8mo <- create_iv_fdi("2019-12-31","2020-09-01",2012,2017)

cgfdi_12_8mo <- cgfdi_12_8mo %>% 
  dplyr::select(cdids,chinanoiv_d_log,chinavaliv_d_log,chinajobsiv_d_log,
                z_log,zval_log,zjobs_log) %>% 
  mutate(year=2012) %>% 
  rename_with(~paste0(., "_8mo"), -c("cdids","year"))

cgfdi_20_8mo <- cgfdi_20_8mo %>%
  dplyr::select(cdids,chinanoiv_d_log,chinavaliv_d_log,chinajobsiv_d_log,
                z_log,zval_log,zjobs_log) %>% 
  mutate(year=2020) %>% 
  rename_with(~paste0(., "_8mo"), -c("cdids","year"))

cfditime_8mo <- rbind(cgfdi_12_8mo,cgfdi_20_8mo)

### 1 year 
cgfdi_12_1y <- create_iv_fdi("2010-12-31","2012-09-01",2004,2009)
cgfdi_20_1y <- create_iv_fdi("2018-12-31","2020-09-01",2012,2017)

cgfdi_12_1y <- cgfdi_12_1y %>% 
  dplyr::select(cdids,chinanoiv_d_log,chinavaliv_d_log,chinajobsiv_d_log,
                z_log,zval_log,zjobs_log) %>%
  mutate(year=2012) %>% 
  rename_with(~paste0(., "_1y"), -c("cdids","year"))

cgfdi_20_1y <- cgfdi_20_1y %>% 
  dplyr::select(cdids,chinanoiv_d_log,chinavaliv_d_log,chinajobsiv_d_log,
                z_log,zval_log,zjobs_log) %>% 
  mutate(year=2020) %>% 
  rename_with(~paste0(., "_1y"), -c("cdids","year"))

cfditime_1y <- rbind(cgfdi_12_1y,cgfdi_20_1y)

### 3 years 
cgfdi_12_3y <- create_iv_fdi("2008-12-31","2012-09-01",2004,2009)
cgfdi_20_3y <- create_iv_fdi("2016-12-31","2020-09-01",2012,2017)

cgfdi_12_3y <- cgfdi_12_3y %>%
  dplyr::select(cdids,chinanoiv_d_log,chinavaliv_d_log,chinajobsiv_d_log,
                z_log,zval_log,zjobs_log) %>% 
  mutate(year=2012) %>% 
  rename_with(~paste0(., "_3y"), -c("cdids","year"))

cgfdi_20_3y <- cgfdi_20_3y %>% 
  dplyr::select(cdids,chinanoiv_d_log,chinavaliv_d_log,chinajobsiv_d_log,
                z_log,zval_log,zjobs_log) %>% 
  mutate(year=2020) %>% 
  rename_with(~paste0(., "_3y"), -c("cdids","year"))

cfditime_3y <- rbind(cgfdi_12_3y,cgfdi_20_3y)

anesdistricttime2 <- anesdistricttime2 %>%
  left_join(.,cfditime_8mo) %>% 
  left_join(.,cfditime_1y) %>% 
  left_join(.,cfditime_3y)
  

anesdistricttime2 <- anesdistricttime2 %>% 
  mutate(sample_state=as.factor(sample_state),
         year=as.factor(year),
         cdids=as.factor(cdids),
         race=factor(race, levels=c(1,2,3,4))) %>%  
  mutate(chinafdidum=ifelse(ch_cty_d>0,1,0),
         chinalldum=ifelse(chinano_d>0,1,0)) %>% 
  # Turn to over 100 
  mutate(across(c(unemploy_ivbase_d,unemploy_initbase_d,unemploy_diff_d,
                  perchinese_ivbase_d,perchinese_initbase_d,chinesepop_diff_d,
                  per_manu_ivbase_d,per_manu_initbase_d,
                  percollege_ivbase_d,percollege_initbase_d),~.*100))  %>% 
  mutate(across(c(pop_initbase_d,pop_ivbase_d,medinc_ivbase_d,medinc_initbase_d),list(log = ~log(.))))



saveRDS(anesdistricttime2,file="anes1220analysisIV.Rds")
write.dta(anesdistricttime2,file="anes1220analysisIV.dta")

## ANES 2012 data frames 

df12 <- anesdistricttime2 %>% 
  filter(year==2012)

# add occupation and news consumption data 
occ <- readRDS("anes12occdata.Rds")

news <- readRDS("anes12newscons.Rds")
anesdk <- anes12 %>% 
  dplyr::select(caseid,chinadk)

df12occ <- left_join(df12,occ) %>% 
  left_join(.,news) %>% 
  left_join(.,anesdk)

write.dta(df12occ,file="anes12analysis.dta")
