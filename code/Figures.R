##################
# China GFDI MA paper 
# Date: 2/1/2023
# Author: Aycan Katitas 
# Figures - R&R 
##################

##################
### setwd and load libraries 

setwd("~/Dropbox/UVA stuff/Methods Paper/Paper/Newsubmit/R&R")

library(dplyr)
library(tidyverse)
library(lfe)
library(lme4)
library(stargazer)
library(broom)
library(arm)
library(fixest)
library(marginaleffects)
library(viridis)
library(readstata13)
library(concordance)
library(scales)

################# Simulation Configuration 

set.seed(145)
n.sims <- 1000
n=100

################# Load data 
anesdistricttime2 <- readRDS("anes1220analysisIV.Rds")

load("usfdi.Rdata")

chinama <- read.csv("chinama.csv") 



################# Figures 

# Figure A1 - ANES 2012 and ANES 2020 China threat 

anesdistricttime2 %>% 
  mutate(chectg=ifelse(year==2012&chinaord==0,"Good",
                       ifelse(year==2012&chinaord==1,"Neither Good or Bad",
                              ifelse(year==2012&chinaord==2,"Bad",
                                     ifelse(year==2020&chinaord==0,"No Threat",
                                            ifelse(year==2020&chinaord==1,"Little Threat",
                                                   ifelse(year==2020&chinaord==2,"Moderate Threat",
                                                          ifelse(year==2020&chinaord==3,"A Lot of Threat",
                                                                 ifelse(year==2020&chinaord==4,"A Great Deal of Threat",NA)))))))),
         year=ifelse(year==2012,"ANES 2012","ANES 2020")) %>% 
  dplyr::select(caseid,year,chectg) %>%
  drop_na() %>% 
  mutate(chectg=fct_relevel(chectg,c("Good","Neither Good or Bad","Bad","No Threat",
                                     "Little Threat","Moderate Threat","A Lot of Threat","A Great Deal of Threat"))) %>%
  group_by(year,chectg) %>% 
  summarise(n=n()) %>% 
  group_by(year) %>% 
  mutate(perc=100*n/sum(n)) %>% 
  ggplot(aes(chectg,perc)) +
  geom_bar(stat="identity") +
  facet_wrap(~year,
             scales="free") + 
  theme_classic(base_size=66) +
  xlab("Is China a Threat?") +
  ylab("") + 
  theme(axis.text.x = element_text(angle = 45,hjust=1))

ggsave(plot=last_plot(),filename="output/figa1.png",
       width = 45, height = 25, units = "in")

# Figure A5 - China Jobs X Year
jobsyear <- read.csv("marginsjobsyear.csv")

jobsyear %>% 
  mutate(year=ifelse(year==2012,"ANES 2012","ANES 2020"),
         year=factor(year,levels=c("ANES 2012","ANES 2020"))) %>% 
  ggplot(aes(x=chinajobsiv_d_log,y=estimate*100,ymin=conflow*100,ymax=confhigh*100,fill=year)) +
  geom_ribbon() + 
  geom_line(aes(linetype=year),color="black",size=4) + 
  #geom_hline(yintercept=50,linetype="dashed",size=2) +
  facet_wrap(~year) +
  ylab("Pr(China as a Threat)") +
  xlab("Chinese GFDI Jobs (logged)") +
  theme_classic(base_size=66) +
  scale_linetype_manual(values=c("solid","solid")) + 
  scale_fill_manual(values=c("azure3","azure4")) + 
  guides(fill="none")  +
  theme(legend.position="none") + 
  scale_x_continuous(breaks=seq(min(jobsyear$chinajobsiv_d_log),max(jobsyear$chinajobsiv_d_log),1)) + 
  ylim(0,75)

ggsave(plot=last_plot(),filename="output/figa5.png",
       width = 45, height = 25, units = "in")

# Figure A6 - China Jobs x Nationalism 
natjobs <- read.csv("natmargins.csv")

natjobs %>% 
  mutate(nationalism=ifelse(nationalism==0,"Low",
                            ifelse(nationalism==2,"Medium","High")),
         nationalism=factor(nationalism, levels=c("Low","Medium","High"))) %>% 
  ggplot(aes(x=chinajobsiv_d_log,y=est*100,ymin=conflow*100,ymax=confhigh*100,fill=nationalism)) +
  geom_ribbon() + 
  geom_line(aes(linetype=nationalism),color="black",size=4) + 
  facet_wrap(~nationalism) +
  ylab("Pr(China as a Threat)") +
  xlab("Chinese GFDI Jobs (logged)") +
  theme_classic(base_size=66) +
  scale_linetype_manual(values=c("solid","solid","solid")) + 
  scale_fill_manual(values=c("azure3","azure3","azure3")) + 
  guides(fill="none")  +
  theme(legend.position="none") + 
  scale_x_continuous(breaks=seq(min(natjobs$chinajobsiv_d_log),max(natjobs$chinajobsiv_d_log),1))

ggsave(plot=last_plot(),filename="output/figa6.png",
       width = 45, height = 25, units = "in")




### Appendix Figures 

## Figure A1 - Chinese FDI BOP + greenfield + M&A 

# annual Chinese GFDI


anchina <- usfdifips %>%
  filter(source.country=="China") %>%  
  group_by(Year) %>% 
  summarise(chinano=n(),
            chinaval=sum(value_2010,na.rm=TRUE),
            chinajobs=sum(jobs,na.rm=TRUE)) %>% 
  filter(Year!=2021)

# annual Chinese M&A 
chinama <- read.csv("china20002012.csv",skip=1,na.strings=c("","NA"),stringsAsFactors = FALSE)

chinama <- chinama %>% 
  dplyr::select(-X)

chinama2 <- read.csv("china1321.csv",skip=1,na.strings=c("","NA"),stringsAsFactors = FALSE)

chinaanma <- rbind(chinama,chinama2)

chinaanmaf <- chinaanma %>% 
  separate(X..Date.Announced,c("month","day","year"),sep="/") %>%
  mutate(Year=paste0("20",year)) %>% 
  count(Year) %>% 
  filter(Year!="2021"&Year!="20NA") %>%
  rename(ma=n) %>%
  mutate(Year=as.numeric(Year))
  
chinaan <- left_join(anchina,chinaanmaf)

# china bop 
chbop <- read.csv("chinabopposition.csv",skip=5)

chbop2 <- chbop %>% 
  mutate(X=trimws(X)) %>% 
  filter(X=="China") %>%
  pivot_longer(cols=X2000:X2020,
               names_to="Year",
               values_to="bop") %>% 
  mutate(Year=as.numeric(trimws((gsub("X","",Year))))) %>% 
  dplyr::select(-X)

chinaan2 <- left_join(chinaan,chbop2) %>%
  pivot_wider(names_from="Year",
              values_from=chinano:bop) %>% 
  pivot_longer(cols=chinano_2004:bop_2020,
               names_to="group",
               values_to="value") %>% 
  mutate(group=gsub("china","",group)) %>% 
  separate(group,c("group","Year"),sep="_") %>% 
  mutate(value=as.numeric(value),
         Year=as.numeric(Year))

# figure bop gfdi 
chinaan2 %>%
  filter(group=="bop"|group=="val") %>% 
  ggplot(aes(x=Year,y=value,group=group,color=group)) + 
  geom_line(size=2) +
  scale_color_manual(values=c("black","green")) +
  ylab("$ (millions)") +
  theme_classic(base_size = 56) +
  theme(axis.title.y = element_text(vjust = 1, angle = 360,
                                    margin=margin(t=0,r=20,b=0,l=40)),
        legend.position="none") + 
  scale_x_continuous(breaks=seq(min(chinaan2$Year),max(chinaan2$Year),1)) +
  annotate("text",x=2019.5,y=37000,label="FDI Position",size=15) +
  annotate("text",x=2019.5,y=1000,label="Greenfield FDI Value",size=15) +
  coord_cartesian(xlim = c(2004, 2020), 
                  clip = 'off')

ggsave(plot=last_plot(),filename="output2/figurebopgfdi.png",
       width = 45, height = 25, units = "in",limitsize=FALSE) 

# Chinese GFDI M&A 
chinaan2 %>%
  filter(group=="no"|group=="ma") %>% 
  ggplot(aes(x=Year,y=value,group=group,color=group)) + 
  geom_line(size=2) +
  scale_color_manual(values=c("red","green")) +
  ylab("$ (millions)") +
  theme_classic(base_size = 56) +
  theme(legend.position="none") + 
  scale_x_continuous(breaks=seq(min(chinaan2$Year),max(chinaan2$Year),1)) + 
  annotate("text",x=2020,y=60,label="Greenfield",size=15) +
  annotate("text",x=2020,y=40,label="M&A",size=15) +
  coord_cartesian(xlim = c(2004, 2020), 
                  clip = 'off') + 
  ylab("Number of Chinese Greenfield FDI/Mergers and Acquisitions")

ggsave(plot=last_plot(),filename="output2/figuregfdima.png",
       width = 35, height = 25, units = "in",limitsize=FALSE) 

## Figure A3 - US congressional district map for Chinese GFDI 

# get 2020 congressional district map
library(USAboundaries)
library(sf)
library(leaflet)
library(concordance)

congress_sf <- us_congressional(resolution = 'low')



# proportion 2004-2020 gfdi to 2020 CD 

ctycd <- read.csv("countycd116.csv",skip=1)

names(ctycd) <- c("FIPS","statefips","cd116","stateab","countryname","pop20","afact")

chinamap <- usfdifips %>% 
  filter(source.country=="China") %>% 
  count(FIPS) %>%
  rename(count=n) %>% 
  #group_by(FIPS) %>% 
  #summarise(val=sum(value_2010,na.rm=T)) %>% 
  left_join(ctycd) %>% 
  mutate(cdids=paste0(stateab,cd116),
         wcount=count*afact) %>% 
  group_by(cdids) %>% 
  summarise(count=sum(wcount,na.rm=T))
  
cdmap <- congress_sf %>% 
  mutate(cdids=paste0(state_abbr,as.numeric(cd116fp))) %>%
  left_join(chinamap) %>% 
  arrange(cdids) %>%
  mutate(count=ifelse(is.na(count),0,count)) %>%
  filter(state_abbr!="HI"&state_abbr!="AK"&state_abbr!="PR")

ggplot(cdmap,aes(fill=count))+
  geom_sf() +
  theme_void() + 
  scale_fill_distiller(palette = "Reds", 
                       direction = 1,name="",
                       breaks=c(0,25),
                       labels=c("Low","High")) +
  theme(legend.position="bottom",
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(size=30))

ggsave("output2/map.png",width=6,height=6,dpi="screen")
ggsave(plot=last_plot(),filename="output2/figurechinamap.png",
       width = 25, height = 25, units = "in",limitsize=FALSE) 


## Figure A4 - Industry Differences Between Greenfield and M&A 

crossnaics <- read.csv("fdimarketsnaics.csv")

indnames <- read.csv("naics2names.csv") %>% 
  mutate(naics2=as.numeric(naics2))

cnaics <- crossnaics %>% 
  dplyr::select(-Sector_id,-X) %>%
  mutate(ind.sector=tolower(trimws(ind.sector)),
         subsector=tolower(trimws(subsector)))
  

industrychina <- usfdifips %>%
  filter(Date<"2020-09-01") %>% 
  filter(source.country=="China") %>% 
  arrange(Date) %>% 
  mutate(ind.sector=tolower(trimws(ind.sector)),
         subsector=tolower(trimws(subsector))) %>% 
  mutate(subsector=ifelse(subsector=="motorcycle, bicycle, & parts","motorcyle, bicycle, & parts",
                          ifelse(subsector=="clothing & clothing accessories","clothing & clothing accessories stores",
                                 ifelse(subsector=="furniture, homeware & related products (wood products)",
                                        "furniture, homeware & related products",
                                        ifelse(subsector=="furniture, homeware & related products (consumer products)",
                                               "furniture, homeware & related products",
                                               ifelse(subsector=="accommodation","accomodation",
                                                      ifelse(subsector=="furniture, homeware & related products (textiles)",
                                                             "furniture, homeware & related products",
                                                             ifelse(subsector=="motor vehicle & parts dealers (automotive components)",
                                                                    "motor vehicle & parts dealers",
                                                                    ifelse(subsector=="audio & video equipment (electronic components)",
                                                                           "audio & video equipment",subsector)))))))),
         ind.sector=ifelse(ind.sector=="industrial equipment","industrial machinery, equipment & tools",
                           ifelse(ind.sector=="warehousing","warehousing & storage",
                                  ifelse(ind.sector=="coal, oil & gas","coal, oil and natural gas",
                                         ifelse(ind.sector=="food & beverages","food & tobacco",
                                                ifelse(ind.sector=="transportation & warehousing","transportation",
                                                       ifelse(ind.sector=="building materials","building & construction materials",
                                                              ifelse(ind.sector=="renewable energy","alternative/renewable energy",ind.sector
                                                              )))))))) %>% 
  left_join(cnaics) %>% 
  dplyr::select(Date,investing.comp,ind.sector,subsector,naics) %>% 
  mutate(naics2=as.numeric(substr(as.character(naics),1,2))) %>% 
  mutate(naics2=ifelse(naics2==32|naics2==33,31,
                       ifelse(naics2==45,44,
                              ifelse(naics2==49,48,naics2)))) %>% 
  left_join(indnames) # 21 obs that didn't match with the industry names 
  

# sic naics crosswalk - dont match 1 to 1 
sicnaics <- read.csv("SIC4_to_NAICS6.csv") %>%  group_by(SIC4) %>% 
  filter(Emp_weight==max(Emp_weight)) %>% 
  rename(target.sic=SIC4,
         naics=NAICS6) %>% 
  dplyr::select(target.sic,naics)


chinamaind <- chinama %>% 
  mutate(year=as.numeric(substr(dateann,7,8))) %>% 
  filter(year>3&year<21) %>% 
  dplyr::select(dateann,target.name,target.sic,target.state) %>% 
  mutate(target.sic=as.numeric(target.sic)) %>% 
  filter(!is.na(target.sic)) %>% 
  left_join(sicnaics) %>%
  mutate(naics=ifelse(target.sic==1011,212210,
                      ifelse(target.sic==1021,212230,
                             ifelse(target.sic==1041,212221,
                                    ifelse(target.sic==1044,212222,
                                           ifelse(target.sic==1061,212230,
                                                  ifelse(target.sic==1094,212291,
                                                         ifelse(target.sic==1221,212111,
                                                                ifelse(target.sic==1241,213113,
                                                                       ifelse(target.sic==1381,213111,
                                                                              ifelse(target.sic==1382,213112,
                                                                                     ifelse(target.sic==1389,213112,
                                                                                            ifelse(target.sic==1522,236118,
                                                                                                   ifelse(target.sic==1711,238210,
                                                                                                          ifelse(target.sic==1781,237110,
                                                                                                                 ifelse(target.sic==2011,311611,
                                                                                                                        ifelse(target.sic==2013,311612,
                                                                                                                               ifelse(target.sic==2023,311511,
                                                                                                                                      ifelse(target.sic==4922,486210,
                                                                                                                                             ifelse(target.sic==4953,562920,
                                                                                                                                                    ifelse(target.sic==6029,522110,
                                                                                                                                                           ifelse(target.sic==6726,525990,
                                                                                                                                                                  ifelse(target.sic==8051,623110,
                                                                                                                                                                         ifelse(target.sic==8211,611110,
                                                                                                                                                                                ifelse(target.sic==8221,611310,
                                                                                                                                                                                       ifelse(target.sic==9511,924110,
                                                                                                                                                                                              ifelse(target.sic==1311,211120,naics
                                                                                                                                                                                       ))))))))))))))))))))))))))) %>%
  mutate(naics2=as.numeric(substr(as.character(naics),1,2))) %>% 
  mutate(naics2=ifelse(naics2==32|naics2==33,31,
                       ifelse(naics2==45,44,
                              ifelse(naics2==49,48,naics2)))) %>% 
  left_join(indnames) 

indgren <- industrychina %>% 
  add_count() %>% 
  rename(allcount=n) %>% 
  count(naics2,indname,allcount) %>% 
  rename(count=n) %>%
  mutate(per=count/allcount) %>% 
  mutate(group="green")

indma <- chinamaind %>% 
  add_count() %>% 
  rename(allcount=n) %>% 
  count(naics2,indname,allcount) %>% 
  rename(count=n) %>%
  mutate(per=count/allcount) %>%
  mutate(group="ma")

indgraph <- rbind(indma,indgren)

indgraph %>%  
  filter(!is.na(indname)) %>% 
  mutate(indname=ifelse(indname=="Administrative and Support and Waste Management and Remediation Services",
                        "Administrative/Support/Waste Management/Remediation Services",
                        ifelse(indname=="Professional, Scientific, and Technical Services","Scientific/Technical Services",
                               indname))) %>% 
  ggplot(aes(reorder(indname,-per),per,fill=group)) +
  geom_bar(stat="identity",position="dodge") + 
  theme_classic(base_size = 56) +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab("") + 
  ylab("%") + 
  scale_fill_manual(values=c("green","red"), 
                    labels=c("Greenfield","M&A"))

ggsave(plot=last_plot(),filename="output2/indgraph.png",
       width = 40, height = 35, units = "in") 

## CFIUS coding of critical industries - 2009 CFIUS report

natsec <- c(314,324,325,326,327,331,332,333,334,335,336,339,
            511,517,518,519,523,525,541,561,562,
            211,212,213,221,236,237,238,
            423,424,443,483,484,485,488,493)

indma2 <- chinamaind %>% 
  dplyr::select(dateann,target.name,target.state,naics,indname) %>% 
  mutate(naics3=substr(as.character(naics),1,3))  %>% 
  mutate(natsec=ifelse(naics3 %in% natsec,"sensitive","not sensitive")) %>%  
  add_count() %>% 
  rename(total=n) %>% 
  count(total,natsec) %>%  
  rename(count=n) %>% 
  mutate(per=count/total) %>% 
  mutate(group="ma")


indgren2 <- industrychina %>% 
  mutate(naics3=as.numeric(substr(as.character(naics),1,3))) %>% 
  mutate(naics3=str_pad(naics3, width=3, side="right", pad="0")) %>% 
  mutate(natsec=ifelse(naics3 %in% natsec,"sensitive","not sensitive"))%>% 
  add_count() %>% 
  rename(total=n) %>% 
  count(total,natsec) %>% 
  rename(count=n) %>% 
  mutate(per=count/total) %>% 
  mutate(group="green")

indgraph2 <- rbind(indma2,indgren2)

indgraph2 %>%  
  ggplot(aes(reorder(natsec,-per),per,fill=group)) +
  geom_bar(stat="identity",position="dodge") + 
  theme_classic(base_size = 56) + 
  theme(legend.title=element_blank(),
        legend.position="bottom") + 
  xlab("") + 
  ylab("%") +
  scale_fill_manual(values=c("green", "red"), 
                    labels=c("Greenfield","M&A")) +
  scale_x_discrete(labels=c("Not Sensitive Industry","Sensitive Industry"))

ggsave(plot=last_plot(),filename="output2/indgraphnatsec.png",
       width = 20, height = 15, units = "in") 



# Figure A6 - MediaJobs x Nationalism 
anesdistricttime2$year <- as.factor(anesdistricttime2$year)


m3 <- feols(chinaect ~ education + 
              econoutlook + party + age + gender +
              unemploy_d + permanu_d + perchinese_d + medinct_d +
              mediajobs_d_log*nationalism
            | sample_state+year , cluster=~sample_state, 
            data = anesdistricttime2)

df <- datagrid(model=m3,nationalism=0:4,
               mediajobs_d_log=seq(min(anesdistricttime2$mediajobs_d_log,na.rm=T),
                                   max(anesdistricttime2$mediajobs_d_log,na.rm=T),by=0.1),
               grid.type="counterfactual")

pred <- predictions(m3,newdata=df) %>% 
  group_by(nationalism,mediajobs_d_log) %>% 
  summarise(across(c(predicted,std.error), mean,na.rm=T)) %>%
  mutate(conf.high=predicted+1.96*(std.error),
         conf.low=predicted-1.96*(std.error)) %>% 
  mutate(nationalism=as.factor(nationalism))

pred2 <- pred %>% 
  filter(nationalism==0|nationalism==2|nationalism==4) %>% 
  mutate(nationalism=ifelse(nationalism==0,"Low",
                            ifelse(nationalism==2,"Medium","High")))

pred2$nationalism <- factor(pred2$nationalism, levels=c("Low","Medium","High"))


ggplot(pred2, aes(x=mediajobs_d_log,ymin=conf.low,ymax=conf.high,y=predicted), color=black, size=2)+
  geom_line(size=2) + 
  geom_ribbon(alpha=0.15) + 
  facet_wrap(~nationalism) +
  theme_classic(base_size = 56) + 
  xlab("News Articles on Jobs Created by Chinese Greenfield FDI (logged)") + 
  ylab("Pr(China as a Threat)") +
  scale_y_continuous(breaks=seq(0.20,0.70,0.2)) +
  scale_x_continuous(breaks=seq(-3,7,1))


ggsave(plot=last_plot(),filename="output/figurea1.png",
       width = 25, height = 15, units = "in")


