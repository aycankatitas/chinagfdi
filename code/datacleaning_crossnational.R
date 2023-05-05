##################
# China GFDI MA paper 
# Date: 1/20/2023 
# Author: Aycan Katitas 
# Data Cleaning and Merging for Cross-Sectional Analysis - for R&R
##################

setwd("~/Dropbox/UVA stuff/Methods Paper/Paper/Newsubmit/R&R")

library(dplyr)
library(countrycode)
library(foreign)
library(readstata13)
library(quantmod)

## Load fdi Markets FDI dataset

load("fdi0319.Rdata")


names(fdi) <- c("Date","investing.comp","parent.comp","source.country","source.state","source.city","dest.country",
                "dest.state","admin.region","dest.city","ind.sector","subsector","cluster","ind.activity","value",
                "estimatedv","jobs","estimatedj","project.type")


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

## Filter source countries as included in Rose (2016) dataset

globalfdi <- fdi2 %>% 
  filter(source.country=="China"|source.country=="United States"|source.country=="United Kingdom"|
           source.country=="France"|source.country=="Germany"|source.country=="Canada"|
           source.country=="Japan"|source.country=="South Africa"|source.country=="Brazil"|
           source.country=="Venezuela"|source.country=="Iran"|source.country=="Israel"|
           source.country=="India"|source.country=="South Korea"|source.country=="Pakistan"|
           source.country=="Russia") %>% 
  filter(Year<2014&Year>2001) %>% 
  group_by(Year,source.country,dest.country) %>% 
  summarise(count=n(),
            val=sum(value_2010,na.rm=T),
            jobs=sum(jobs,na.rm=T)) %>% 
  # create imf country codes using country names
  mutate(cty1=countrycode(source.country,origin="country.name",destination="imf"),
    cty2=countrycode(dest.country,origin="country.name",destination="imf")) %>% 
  # small countries were not created - drop 
  filter(!is.na(cty2)) %>% 
  rename(year=Year)

# import Rose's dataset 

rose <- read.dta13("data1.dta")

logplus <- function(x){
  d <- log(x+0.1)
}


rose2 <- rose %>%
  filter(!(cty1==954)) %>% 
  mutate(year=year(year)) %>% 
  filter(year>2002) %>% 
  left_join(globalfdi) %>% 
  mutate(count=ifelse(is.na(count)&year>2002,0,count),
    val=ifelse(is.na(val)&year>2002,0,val),
         jobs=ifelse(is.na(jobs)&year>2002,0,jobs),
    neg_100=neg/100) %>% 
  mutate_at(vars("count","val","jobs","pos","neg","unf","fav"),list(log = ~logplus(.)))

write.dta(rose2,file="rosefdi.dta")

rose3 <- rose %>% 
  mutate(year=year(year)) %>% 
  left_join(globalfdi) %>% 
  mutate(count=ifelse(is.na(count)&year>2002,0,count),
         val=ifelse(is.na(val)&year>2002,0,val),
         jobs=ifelse(is.na(jobs)&year>2002,0,jobs),
         neg_100=neg/100) %>% 
  mutate_at(vars("count","val","jobs","pos","neg","unf","fav"),list(log = ~logplus(.)))

write.dta(rose3,file="rosefdi2.dta")



  
  

  
  
  
  
  
  
  
  
  
  

  
  