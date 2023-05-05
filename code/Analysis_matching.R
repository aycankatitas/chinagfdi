##################
# China GFDI MA paper 
# Date: 04/11/2023
# Author: Aycan Katitas 
# Matching - R&R 
##################

##################
### setwd and load libraries 

library(dplyr)
library(tidyverse)
library(lfe)
library(lme4)
library(stargazer)
library(broom)
library(lmtest)
library(sandwich)
library(MatchIt)
library(optmatch)
library(modelsummary)
library(kableExtra)
library(cobalt)



setwd("~/Dropbox/UVA stuff/Methods Paper/Paper/Newsubmit/R&R")

anesdistricttime2 <- readRDS("anes1220analysisIV.Rds")

## Matching


## Prepare the dataset for matching 
## Treatment - Dummy variable if district has received Chinese GFDI 
## Create dummy variables for i) district has received any other GFDI ii) district past FDI status
## Collapse DV - Chinese threat perceptions - at the district level
matchdf <- anesdistricttime2 %>%
  mutate(otherdum=ifelse(othernoiv_d>0,1,0),
         allpast=ifelse(allgfdi_base_d>0,1,0)
         ) %>%
  dplyr::select(sample_state,year,cdids,chinafdidum,otherdum,chinaect,
                unemploy_ivbase_d,per_manu_ivbase_d,perchinese_ivbase_d,medinc_ivbase_d_log,allpast
                )%>%
  mutate(cd_empty=ifelse(grepl("99",cdids),1,0)) %>%
  filter(cd_empty==0) %>%
  filter(!is.na(chinaect)) 

mdf <- matchdf %>%
  group_by(sample_state,cdids,year,chinafdidum,otherdum,allpast) %>%
  summarise_all(mean,na.rm=T) %>%
  arrange(cdids,year) %>%
  na.omit()

## Propensity score matching with replacement

m <- matchit(chinafdidum ~ unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log,
                     method="nearest",
             exact=c("year","otherdum","allpast"),
             #k2k=TRUE,
                     data=mdf,
             replace=TRUE)


b <- bal.tab(m,un=TRUE)

print(b)
#bal.plot(m,var.name="perchinese_ivbase_d",which="both")
#love.plot(bal.tab(m),stat="mean.diffs",abs=F)

## Matching performance
m
summary(m)

#plot(mm, var.order = "unmatched")

dta_nearest <- match.data(m)

xx <- lm(chinaect ~ chinafdidum+unemploy_ivbase_d +per_manu_ivbase_d +  perchinese_ivbase_d + medinc_ivbase_d_log, data = dta_nearest, weights = weights)
summary(xx)

models <- list(
  "OLS" = lm(chinaect ~ chinafdidum, data = dta_nearest, weights = weights),
  "OLS" = lm(chinaect ~ chinafdidum+unemploy_ivbase_d +per_manu_ivbase_d +  perchinese_ivbase_d + medinc_ivbase_d_log, data = dta_nearest, weights = weights)
)

modelsummary(models,
             output="results/matchingtable.docx",
             coef_omit="Intercept",
             stars=TRUE,
             gof_omit='DF|Deviance|R2|AIC|BIC|F|RMSE',
             gof_map=NA,
             coef_rename=c(chinafdidum="China FDI==1",
                           unemploy_ivbase_d="% Unemployment",
                           per_manu_ivbase_d="% Manufacturing",
                           perchinese_ivbase_d="% Chinese",
                           medinc_ivbase_d_log="Median Income (logged)"),
             title="District-Level Propensity Score Matching Estimates of Chinese Threat Perceptions",
             notes="Note: OLS regression on the mathced sample using propensity score matching with replacement weights.")



#quantile(mdf$medinc_ivbase_d_log,c(0.25,0.5,0.75,0.9))
## CEM 
mod_match <- matchit(chinafdidum ~ unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                     +year+otherdum+allpast,
                     method="cem",
                    #k2k=TRUE,
                     cutpoints=list(unemploy_ivbase_d=c(4.32,6.14,9.04,11.08),
                                    per_manu_ivbase_d=c(5.07,8.04,11.35,15.61),
                                    perchinese_ivbase_d=c(0.18,0.31,0.64,1.40),
                                    #medinc_ivbase_d=c(46456,53125,64248,77214),
                                    medinc_ivbase_d_log=c(10.75,10.89,11.07,11.27)),
                       data=mdf)

mod_match
summary(mod_match)

b2 <- bal.tab(mod_match,un=TRUE)
print(b2)

dta_c <- match.data(mod_match)

dta_c <- dta_c %>% 
  arrange(subclass,chinafdidum) 

# Each variable is divided into 5 groups: Below 25%, 25% to 50%, 50% to 75%, 75% to 90%, above 90%
# Total number of cells is therefore 5*5*5*5 = 625.
# total # of cells - observation in each cell
unique(dta_c$subclass) %>% length() # subclass = ID for cell that observation comes from
table(dta_c$subclass)

# To estimate treatment effects, with weights and clustering

models2 <- list(
  "OLS" = lm(chinaect ~ chinafdidum, data = dta_c, weights = weights),
  "OLS" = lm(chinaect ~ chinafdidum+unemploy_ivbase_d +per_manu_ivbase_d + 
               perchinese_ivbase_d + medinc_ivbase_d_log, data = dta_c, weights = weights)
)

modelsummary(models2,
             vcov=~subclass,
             output="results/matchingtablecem.docx",
             coef_omit="Intercept",
             stars=TRUE,
             gof_omit='DF|Deviance|R2|AIC|BIC|F|RMSE',
             gof_map=NA,
             coef_rename=c(chinafdidum="China FDI==1",
                           unemploy_ivbase_d="% Unemployment",
                           per_manu_ivbase_d="% Manufacturing",
                           perchinese_ivbase_d="% Chinese",
                           medinc_ivbase_d_log="Median Income (logged)"),
             title="District-Level Coarsened Exact Matching Estimates of Chinese Threat Perceptions",
             notes="Note: OLS regressions on the matched sample using coarsened exact matching (cem) weights. 
             Standard errors are clustered around cem subclasses.")

