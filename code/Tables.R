##################
# China GFDI MA paper 
# Date: 01/21/2023
# Author: Aycan Katitas 
# Tables - R&R 
##################

##################
### setwd and load libraries 

library(dplyr)
library(tidyverse)
library(lfe)
library(lme4)
library(stargazer)
library(broom)

setwd("~/Dropbox/UVA stuff/Methods Paper/Paper/Newsubmit/R&R")

################# Functions 

#For Stargazer
replaceVarName <- function(var.vec, var.df){
  # Prepare output vector
  out.vec <- rep(NA, length(var.vec))
  matches <- match(var.vec, var.df$var)
  out.vec <- var.df[matches,]$var_name
  
  if(any(is.na(out.vec))){
    warning(paste("Variable concordence missing: ", 
                  paste(var.vec[is.na(out.vec)], collapse = ", "), 
                  sep = ""))
  } else{
    #print("All variables successfully converted")
  }
  
  return(out.vec)
}

# Create tables function 


createTable <- function(results,var.order,savename,dep.var,addto,title){
  
  var.label <- str_replace_all(var.order, "\\^", "")
  var.label <- str_replace_all(var.label, "\\$", "")
  
  sink(savename)
  stargazer(results,
            #p = pvalue.main, 
            p.auto = FALSE,
            t.auto = FALSE,
            ci = FALSE,
            report = ("vc*s"),
            digits = 3, 
            type = "latex",
            #type = "text",
            title = title,
            dep.var.labels = dep.var,
            model.names = FALSE,
            #column.labels = mod.names.main,
            order = var.order,
            omit=c(omitvars,"Constant"),
            covariate.labels = replaceVarName(var.vec = var.label,
                                              var.df = var.df),
            label = label,
            omit.stat=c("ll","aic","bic","ser","adj.rsq","rsq"),
            add.lines = addto,
            single.row = FALSE,
            df = FALSE,
            font.size = "normalsize",
            star.cutoffs = c(0.1, 0.05, 0.01),
            star.char = c("*", "**", "***"),
            #notes = c("Robust standard errors clustered by ZIP code in parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
            #notes = c("95\\% confidence intervals and p-values are presented. Calculations are based on robust standard errors clustered by ZIP code."),
            notes.align = "l",
            notes.append = FALSE,
            no.space=TRUE,
            dep.var.caption = "")
  sink()
  
  
}

################# Load data 
#anesdistricttime <- readRDS("anes1220analysis.Rds")

anesdistricttime2 <- readRDS("anes1220analysisIV.Rds")

################ Stargazer Configuration 

var.df <- tibble(var = c("chinaect",
                         "nationalism",
                          "education",
                          "econoutlook",
                          "party",
                          "age",
                          "manufall",
                          "gender",
                         "inmedia",
                         "tvmedia",
                         "prmedia",
                         "rdmedia",
                          "unemploy_diff_d",
                         "unemploy_initbase_d",
                         "unemploy_ivbase_d",
                          "per_manu_ivbase_d",
                         "per_manu_initbase_d",
                          "perchinese_ivbase_d",
                         "perchinese_initbase_d",
                         "chinesepop_diff_d",
                          "medinc_ivbase_d_log",
                         "medinc_initbase_d_log",
                         "percollege_ivbase_d",
                         "presvote",
                         "presvote_lag2",
                         "pop_ivbase_d_log",
                         "ch_cty_d",
                         "ch_ctyval_d",
                         "ch_ctyjobs_d",
                         "ch_ctymedia_d",
                         "ch_ctyvalmedia_d",
                         "ch_ctyjobsmedia_d",
                         "ch_ctynomedia_d",
                         "ch_ctyjobsnomedia_d",
                         "ch_ctyvalnomedia_d",
                         "`chinafdidum\\(fit\\)`",
                          "`chinanoiv_d_log\\(fit\\)`",
                          "`chinavaliv_d_log\\(fit\\)`",
                          "`chinajobsiv_d_log\\(fit\\)`",
                         "`chinanoiv_d_log_8mo\\(fit\\)`",
                         "`chinanoiv_d_log_1y\\(fit\\)`",
                         "`chinanoiv_d_log_3y\\(fit\\)`",
                         "`chinavaliv_d_log_8mo\\(fit\\)`",
                         "`chinavaliv_d_log_1y\\(fit\\)`",
                         "`chinavaliv_d_log_3y\\(fit\\)`",
                         "`chinajobsiv_d_log_8mo\\(fit\\)`",
                         "`chinajobsiv_d_log_1y\\(fit\\)`",
                         "`chinajobsiv_d_log_3y\\(fit\\)`",
                         "`medianoiv_d_log\\(fit\\)`",
                         "`mediavaliv_d_log\\(fit\\)`",
                         "`mediajobsiv_d_log\\(fit\\)`",
                         "`nomedianoiv_d_log\\(fit\\)`",
                         "`nomediavaliv_d_log\\(fit\\)`",
                         "`nomediajobsiv_d_log\\(fit\\)`",
                         "z_log",
                         "zval_log",
                         "zjobs_log",
                         "mediano_d_log",
                         "mediaval_d_log",
                         "mediajobs_d_log",
                         "chinanoiv_d",
                         "chinavaliv_d",
                         "chinajobsiv_d",
                         "z",
                         "zval",
                         "zjobs",
                         "mano_d_log",
                         "othernoiv_d",
                         "othervaliv_d",
                         "otherjobsiv_d",
                         "othernoiv_d_log",
                         "othervaliv_d_log",
                         "otherjobsiv_d_log",
                         "`asianoiv_d_log\\(fit\\)`",
                         "`asiavaliv_d_log\\(fit\\)`",
                         "`asiajobsiv_d_log\\(fit\\)`",
                         "`tradeshock_d\\(fit\\)`",
                         "`it_chinanoiv_d_log\\(fit\\)`",
                         "`it_chinavaliv_d_log\\(fit\\)`",
                         "`it_chinajobsiv_d_log\\(fit\\)`",
                         "`ind_chinanoiv_d_log\\(fit\\)`",
                         "`ind_chinavaliv_d_log\\(fit\\)`",
                         "`ind_chinajobsiv_d_log\\(fit\\)`",
                         "`auto_chinanoiv_d_log\\(fit\\)`",
                         "`auto_chinavaliv_d_log\\(fit\\)`",
                         "`auto_chinajobsiv_d_log\\(fit\\)`"),
                  
                  var_name = c("China Threat == 1",
                               "Nationalism",
                               "Education",
                               "Economic Outlook",
                               "Party Identification",
                               "Age",
                               "Works in Manufacturing",
                               "Female",
                               "News from Internet",
                               "News from TV",
                               "News from Newspapers",
                               "News from Radio",
                               "\\delta \\% Unemployment",
                               "\\% Unemployment (Lagged)",
                               "\\% Unemployment",
                               "\\% Manufacturing",
                               "\\% Manufacturing (Lagged)",
                               "\\% Chinese",
                               "\\% Chinese (Lagged)",
                               "\\delta \\% Chinese",
                               "Median Income (logged)",
                               "Median Income (Lagged/logged)",
                               "\\% College",
                               "Republican Pres Vote (t-4)",
                               "Republican Pres Vote (t-8)",
                               "Population (logged)",
                               "China GFDI No",
                               "China GFDI Value",
                               "China GFDI Jobs",
                               "China GFDI News No",
                               "China GFDI News Value",
                               "China GFDI News Jobs",
                               "China GFDI No News No",
                               "China GFDI No News Value",
                               "China GFDI No News Jobs",
                               "China GFDI == 1",
                               "China GFDI No (logged)",
                               "China GFDI Value (logged)",
                               "China GFDI Jobs (logged)",
                               "China GFDI No 8 months (logged)",
                               "China GFDI No 1 year (logged)",
                               "China GFDI No 3 years (logged)",
                               "China GFDI Value 8 months (logged)",
                               "China GFDI Value 1 year (logged)",
                               "China GFDI Value 3 years (logged)",
                               "China GFDI Jobs 8 months (logged)",
                               "China GFDI Jobs 1 year (logged)",
                               "China GFDI Jobs 3 years (logged)",
                               "China GFDI News No (logged)",
                               "China GFDI News Value (logged)",
                               "China GFDI News Jobs (logged)",
                               "China GFDI No News No (logged)",
                               "China GFDI No News Value (logged)",
                               "China GFDI No News Jobs (logged)",
                               "Instrument No (logged)",
                               "Instrument Value (logged)",
                               "Instrument Jobs (logged)",
                               "China GFDI News No (logged)",
                               "China GFDI News Value (logged)",
                               "China GFDI News Jobs (logged)",
                               "China GFDI No",
                               "China GFDI Value",
                               "China GFDI Jobs",
                               "Instrument No",
                               "Instrument Value",
                               "Instrument Jobs",
                               "Chinese M\\&A (logged)",
                               "Other GFDI No",
                               "Other GFDI Value",
                               "Other GFDI Jobs",
                               "Other GFDI No (logged)",
                               "Other GFDI Value (logged)",
                               "Other GFDI Jobs (logged)",
                               "East Asia GFDI No (logged)",
                               "East Asia GFDI Value (logged)",
                               "East Asia GFDI Jobs (logged)",
                               "Trade Shock",
                               "China GFDI No Excluding IT (logged)",
                               "China GFDI Value Excluding IT (logged)",
                               "China GFDI Jobs Excluding IT (logged)",
                               "China GFDI No Excluding Industrial (logged)",
                               "China GFDI Value Excluding Industrial (logged)",
                               "China GFDI Jobs Excluding Industrial (logged)",
                               "China GFDI No Excluding Auto (logged)",
                               "China GFDI Value Excluding Auto (logged)",
                               "China GFDI Jobs Excluding Auto (logged)"))
################# Tables 


  
# Table 1 

ivresa <- felm(chinaect~ nationalism+ education + 
                  econoutlook + party + age + gender + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinanoiv_d_log ~ z_log) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivresb <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinavaliv_d_log ~ zval_log) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivresc <- felm(chinaect ~  nationalism+ education + 
                 econoutlook + party + age + gender  + race +
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinajobsiv_d_log ~ zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivresd <- felm(chinaect ~  nationalism+ education + 
                 econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinafdidum ~ z_log) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

results <- list(ivresa,ivresb,ivresc,ivresd)

var.order <- c("^`chinanoiv_d_log\\(fit\\)`$",
               "^`chinavaliv_d_log\\(fit\\)`$",
               "^`chinajobsiv_d_log\\(fit\\)`$",
               "^`chinafdidum\\(fit\\)`$")

omitvars <-  c("^(Intercept)$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")

savename = "results/table1.txt"
title="Chinese GFDI and Threat Perceptions: Two Stage Least-Squares Estimates"
dep.var="China Threat == 1"
label="tb:base2sls"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes","Yes")),
              c("Individual Level Controls",
                c("Yes","Yes","Yes","Yes")),
              c("District Level Controls",
                c("Yes","Yes","Yes","Yes")))

createTable(results,var.order,savename,dep.var,addto,title)

################# Appendix Tables 

# Table A1 - Descriptive Statistics and Data Sources 
anessum <- anesdistricttime2 %>% 
  dplyr::select(chinaect,ch_cty_d,ch_ctyval_d,ch_ctyjobs_d,
                z,zval,zjobs,
                ch_ctymedia_d,ch_ctyjobsmedia_d,ch_ctyvalmedia_d,
                othernoiv_d,otherjobsiv_d,othervaliv_d,
                nationalism,education,econoutlook,party,age,gender,
                unemploy_ivbase_d, per_manu_ivbase_d,perchinese_ivbase_d,medinc_ivbase_d,
                tradeshock)
                

var.order <- c("^chinaect$",
               "^ch_cty_d$",
               "^ch_ctyval_d$",
               "^ch_ctyjobs_d$",
               "^z$",
               "^zval$",
               "^zjobs$",
               "^ch_ctymedia_d$",
               "^ch_ctyvalmedia_d$",
               "^ch_ctyjobsmedia_d$",
               "^ch_ctynomedia_d$",
               "^ch_ctyvalnomedia_d$",
               "^ch_ctyjobsnomedia_d$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d$",
               "^othernoiv_d$",
               "^otherjobsiv_d$",
               "^othervaliv_d$"
               
)

var.label <- str_replace_all(var.order, "\\^", "")
var.label <- str_replace_all(var.label, "\\$", "")



sink("results/tablea1.txt")
stargazer(as.data.frame(anessum),
          label="tb:appsum",
          digits=2,
          title="SUMMARY STATISTICS AND DATA SOURCES",
          nobs=FALSE,
          omit.summary.stat = c("p25", "p75"),
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df)
          )
sink()
 
# Table A2 - First Stage Results 

fa1 <- felm(chinaect ~ nationalism+ education + 
                   econoutlook + party + age + gender + race + 
                   unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                 | sample_state+year| (chinanoiv_d_log~z_log) |sample_state, # FEs | IVs | Clustered SEs
                 data = anesdistricttime2)
ivfa1 <- fa1$stage1
Fstata1 <- fa1$stage1$iv1fstat$chinanoiv_d_log

fb1 <- felm(chinaect ~ nationalism+ education + 
                   econoutlook + party + age + gender + race + 
                   unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                 | sample_state+year| (chinavaliv_d_log~zval_log) |sample_state, # FEs | IVs | Clustered SEs
                 data = anesdistricttime2)
ivfb1 <- fb1$stage1
Fstatb1 <- fb1$stage1$iv1fstat$chinavaliv_d_log

fc1 <- felm(chinaect ~ nationalism+ education + 
             econoutlook + party + age + gender + race + 
             unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
           | sample_state+year| (chinajobsiv_d_log~zjobs_log) |sample_state, # FEs | IVs | Clustered SEs
           data = anesdistricttime2)
ivfc1 <- fc1$stage1
Fstatc1 <- fc1$stage1$iv1fstat$chinajobsiv_d_log


fd1 <- felm(chinaect ~ nationalism+ education + 
              econoutlook + party + age + gender + race + 
              unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
            | sample_state+year| (chinafdidum~z_log) |sample_state, # FEs | IVs | Clustered SEs
            data = anesdistricttime2)
ivfd1 <- fd1$stage1
Fstatd1 <- fd1$stage1$iv1fstat$chinafdidum

results <- list(ivfa1,ivfb1,ivfc1,ivfd1)
fstats <- c(Fstata1[[5]],Fstatb1[[5]],Fstatc1[[5]],Fstatd1[[5]])

var.orderfs <- c("^z_log$",
                 "^zval_log$",
                 "^zjobs_log$",
                 "^nationalism$",
                 "^education$",
                 "^econoutlook$",
                 "^party$",
                 "^age$",
                 "^gender$",
                 "^unemploy_ivbase_d$",
                 "^per_manu_ivbase_d$",
                 "^perchinese_ivbase_d$",
                 "^medinc_ivbase_d_log$")

omitvars <-  c("^(Intercept)$")

savename="results/tablea2.txt"
dep.var=c("\\shortstack{China GFDI \\\\ No (logged)}",
          "\\shortstack{China GFDI \\\\ Value (logged)}", 
          "\\shortstack{China GFDI \\\\ Jobs (logged)}",
          "China GFDI==1")

label="tb:appfirst"
addto <- list(c("State- Fixed Effects",
                c("Yes","Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes","Yes")),
              c("F-Statistics",
                ceiling(fstats)))



var.label <- str_replace_all(var.orderfs, "\\^", "")
var.label <- str_replace_all(var.label, "\\$", "")

sink(savename)
stargazer(results,
          #p = pvalue.main, 
          p.auto = FALSE,
          t.auto = FALSE,
          ci = FALSE,
          report = ("vc*s"),
          digits = 3, 
          type = "latex",
          #type = "text",
          title = "First Stage Regression Results",
          #dep.var.labels = "",
          model.names = FALSE,
          column.labels = dep.var,
          order = var.orderfs,
          omit=c(omitvars,"Constant"),
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df),
          label = label,
          omit.stat=c("ll","aic","bic","ser","adj.rsq","rsq"),
          add.lines = addto,
          single.row = FALSE,
          df = FALSE,
          font.size = "normalsize",
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("*", "**", "***"),
          #notes = c("Robust standard errors clustered by ZIP code in parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          #notes = c("95\\% confidence intervals and p-values are presented. Calculations are based on robust standard errors clustered by ZIP code."),
          notes.align = "l",
          notes.append = FALSE,
          no.space=TRUE,
          dep.var.caption = "",
          dep.var.labels.include = FALSE)
sink()

# Table A3 - second stage results 


ivresa1 <- felm(chinaect~ nationalism + education + 
                  econoutlook + party + age + gender + race +
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinanoiv_d_log ~ z_log) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivresb1 <- felm(chinaect ~ nationalism + education + 
                  econoutlook+  party + age + gender  + race+
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinavaliv_d_log ~ zval_log) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivresc1 <- felm(chinaect ~ nationalism + education + 
                  econoutlook + party + age + gender + race +
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinajobsiv_d_log ~ zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)


ivresd1 <- felm(chinaect ~ nationalism  + education + 
                  econoutlook + party + age + gender + race +
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinafdidum ~ z_log) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

results <- list(ivresa1,ivresb1,ivresc1,ivresd1)

var.order <- c("^`chinanoiv_d_log\\(fit\\)`$",
               "^`chinavaliv_d_log\\(fit\\)`$",
               "^`chinajobsiv_d_log\\(fit\\)`$",
               "^`chinafdidum\\(fit\\)`$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")
omitvars <-  c("^(Intercept)$")

savename = "results/tablea3.txt"
title="Chinese GFDI and Threat Perceptions: Two Stage Least-Squares Estimates"
dep.var="China Threat == 1"
label="tb:base2sls"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes","Yes")))

createTable(results,var.order,savename,dep.var,addto,title)

# Table A5 - Chinese GFDI Media results 

m1 <- felm(chinaect~ nationalism+ education + 
             econoutlook +  party + age + gender + race + 
             unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
           | sample_state+year| (medianoiv_d_log ~ z_log)  |sample_state, # FEs | IVs | Clustered SEs
           data = anesdistricttime2)

ma1 <- felm(chinaect~ nationalism+ education + 
              econoutlook + party + age + gender + race + 
              unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
            | sample_state+year| (nomedianoiv_d_log ~ z_log)    |sample_state, # FEs | IVs | Clustered SEs
            data = anesdistricttime2)

m2 <- felm(chinaect~ nationalism+ education + 
             econoutlook + party + age + gender + race+ 
             unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
           | sample_state+year| (mediavaliv_d_log ~ zval_log)    |sample_state, # FEs | IVs | Clustered SEs
           data = anesdistricttime2)

ma2 <- felm(chinaect~ nationalism+ education + 
              econoutlook + party + age + gender + race + 
              unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
            | sample_state+year| (nomediavaliv_d_log ~ zval_log)    |sample_state, # FEs | IVs | Clustered SEs
            data = anesdistricttime2)

m3 <- felm(chinaect~ nationalism+ education + 
             econoutlook + party + age + gender + race + 
             unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
           | sample_state+year| (mediajobsiv_d_log ~ zjobs_log)    |sample_state, # FEs | IVs | Clustered SEs
           data = anesdistricttime2)

ma3 <- felm(chinaect~ nationalism+ education + 
              econoutlook + party + age + gender + race + 
              unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
            | sample_state+year| (nomediajobsiv_d_log ~ zjobs_log)    |sample_state, # FEs | IVs | Clustered SEs
            data = anesdistricttime2)


results <- list(m1,ma1,m2,ma2,m3,ma3)

var.order <- c("^`medianoiv_d_log\\(fit\\)`$",
               "^`nomedianoiv_d_log\\(fit\\)`$",
               "^`mediavaliv_d_log\\(fit\\)`$",
               "^`nomediavaliv_d_log\\(fit\\)`$",
               "^`mediajobsiv_d_log\\(fit\\)`$",
               "^`nomediajobsiv_d_log\\(fit\\)`$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")

omitvars <-  c("^(Intercept)$")


savename = "results/tablea5.txt"
title="Chinese GFDI and Threat Perceptions, by Media Reports"
dep.var="China Threat == 1"
label="tb:appmedia"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes","Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes","Yes","Yes","Yes")))

createTable(results,var.order,savename,dep.var,addto,title)


# Table A7 - 2012 only results w/ news consumption 
df12 <- anesdistricttime2 %>% 
  filter(year==2012)

# add occupation and news consumption data 
occ <- readRDS("anes12occdata.Rds")

news <- readRDS("anes12newscons.Rds")

df12occ <- left_join(df12,occ) %>% 
  left_join(.,news)

write.dta(df12occ,file="anes12analysis.dta")


news12a <- felm(chinaect~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + manufall + 
                  inmedia + tvmedia + prmedia + rdmedia +
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state| (chinanoiv_d_log ~z_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = df12occ)

news12b <- felm(chinaect~ nationalism+ education + 
                  econoutlook +  party + age + gender  + race + manufall + 
                  inmedia + tvmedia + prmedia + rdmedia +
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state| (chinavaliv_d_log ~zval_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = df12occ)

news12c <- felm(chinaect~ nationalism+ education + 
                  econoutlook + party + age + gender  +  race + manufall + 
                  inmedia + tvmedia + prmedia + rdmedia + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state| (chinajobsiv_d_log ~zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = df12occ)

results <- list(news12a,news12b,news12c)

var.orderwcont <- c("^`chinanoiv_d_log\\(fit\\)`$",
                    "^`chinavaliv_d_log\\(fit\\)`$",
                    "^`chinajobsiv_d_log\\(fit\\)`$",
                    "^nationalism$",
                    "^education$",
                    "^econoutlook$",
                    "^party$",
                    "^age$",
                    "^gender$",
                    "^inmedia$",
                    "^tvmedia$",
                    "^prmedia$",
                    "^rdmedia$",
                    "^unemploy_ivbase_d$",
                    "^per_manu_ivbase_d$",
                    "^perchinese_ivbase_d$",
                    "^medinc_ivbase_d_log$")

omitvars <-  c("^(Intercept)$")

title = "Chinese GFDI and Threat Perceptions, Controlling for News Consumption"
savename = "results/tablea7.txt"
dep.var="China Economic Threat == 1"
label="tb:app12news"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes")))

createTable(results,var.orderwcont,savename,dep.var,addto,title)


# Table A8 - Chinese M&A - Placebo

mares <- felm(chinaect ~ nationalism+ education + 
                 econoutlook + party + age + gender + race + 
                 unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log +
                 mano_d_log
               | sample_state+year| 0  |sample_state, # FEs | IVs | Clustered SEs
               data = anesdistricttime2)

maresa <- felm(chinaect ~ nationalism+ education + 
                 econoutlook + party + age + gender + race + 
                 unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log +
                 mano_d_log
               | sample_state+year| (chinanoiv_d_log ~ z_log)  |sample_state, # FEs | IVs | Clustered SEs
               data = anesdistricttime2)

maresb <- felm(chinaect ~ nationalism+ education + 
                 econoutlook + party + age + gender + race + 
                 unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log + 
                 mano_d_log
               | sample_state+year| (chinavaliv_d_log ~ zval_log)  |sample_state, # FEs | IVs | Clustered SEs
               data = anesdistricttime2)

maresc <- felm(chinaect ~ nationalism+ education + 
                 econoutlook + party + age + gender + race + 
                 unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log + 
                 mano_d_log
               | sample_state+year| (chinajobsiv_d_log ~ zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
               data = anesdistricttime2)

results <- list(mares,maresa,maresb,maresc)

var.order <- c("^mano_d_log$",
               "^`chinanoiv_d_log\\(fit\\)`$",
               "^`chinavaliv_d_log\\(fit\\)`$",
               "^`chinajobsiv_d_log\\(fit\\)`$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")

omitvars <-  c("^(Intercept)$")

title="Chinese Mergers and Acquisitions and Threat Perceptions"
savename = "results/tablea8.txt"
dep.var="China Threat == 1"
label="tb:appma"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes","Yes")))

createTable(results,var.order,savename,dep.var,addto,title)

## Table A9 Other Asian Countries - Placebo 


asia1 <- felm(chinaect~ nationalism+ education + 
                  econoutlook + party + age + gender + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (asianoiv_d_log ~ asiaz_log) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

asia2 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race +
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (asiavaliv_d_log ~ asiazval_log) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

asia3 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (asiajobsiv_d_log ~ asiazjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

results <- list(asia1,asia2,asia3)

var.order <- c("^`asianoiv_d_log\\(fit\\)`$",
               "^`asiavaliv_d_log\\(fit\\)`$",
               "^`asiajobsiv_d_log\\(fit\\)`$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")

omitvars <-  c("^(Intercept)$")

title="Other East Asian GFDI and Threat Perceptions"
savename = "results/tablea9.txt"
dep.var="China Threat == 1"
label="tb:appasia"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes")))

createTable(results,var.order,savename,dep.var,addto,title)

## Table A10 - China military threat perceptions 

mil1 <- felm(chinamil ~ nationalism+ education + 
                    econoutlook + party + age + gender  + race + 
                    unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                  | sample_state+year| (chinanoiv_d_log ~ z_log)  |sample_state, # FEs | IVs | Clustered SEs
                  data = anesdistricttime2,subset=year==2012)

mil2 <- felm(chinamil ~ nationalism+ education + 
                    econoutlook + party + age + gender  + race + 
                    unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                  | sample_state+year| (chinavaliv_d_log ~ zval_log)  |sample_state, # FEs | IVs | Clustered SEs
                  data = anesdistricttime2,subset=year==2012)

mil3 <- felm(chinamil ~ nationalism+ education + 
                    econoutlook + party + age + gender  + race + 
                    unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                  | sample_state+year| (chinajobsiv_d_log ~ zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                  data = anesdistricttime2,subset=year==2012)

results <- list(mil1,mil2,mil3)

var.order <- c("^`chinanoiv_d_log\\(fit\\)`$",
               "^`chinavaliv_d_log\\(fit\\)`$",
               "^`chinajobsiv_d_log\\(fit\\)`$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")

omitvars <-  c("^(Intercept)$")

title = "Chinese GFDI and Military Threat Perceptions"
savename = "results/tablea10.txt"
dep.var="China Military Threat == 1"
label="tb:appmil"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes")))

createTable(results,var.order,savename,dep.var,addto,title)


# Table A12 - Dropping industries 

ivinda1 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook +   party + age + gender + race +
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (it_chinanoiv_d_log~ it_z_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivinda2 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook +  party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (it_chinavaliv_d_log~ it_zval_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivinda3 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook +  party + age + gender + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (it_chinajobsiv_d_log~ it_zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivindb1 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (ind_chinanoiv_d_log~ ind_z_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivindb2 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (ind_chinavaliv_d_log~ ind_zval_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivindb3 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (ind_chinajobsiv_d_log~ ind_zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivindc1 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (auto_chinanoiv_d_log~ auto_z_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivindc2 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (auto_chinavaliv_d_log~ auto_zval_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

ivindc3 <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log 
                | sample_state+year| (auto_chinajobsiv_d_log~ auto_zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)


results <- list(ivinda1,ivinda2,ivinda3,ivindb1,ivindb2,ivindb3,ivindc1,ivindc2,ivindc3)

var.order <- c("^`it_chinanoiv_d_log\\(fit\\)`$",
               "^`it_chinavaliv_d_log\\(fit\\)`$",
               "^`it_chinajobsiv_d_log\\(fit\\)`$",
               "^`ind_chinanoiv_d_log\\(fit\\)`$",
               "^`ind_chinavaliv_d_log\\(fit\\)`$",
               "^`ind_chinajobsiv_d_log\\(fit\\)`$",
               "^`auto_chinanoiv_d_log\\(fit\\)`$",
               "^`auto_chinavaliv_d_log\\(fit\\)`$",
               "^`auto_chinajobsiv_d_log\\(fit\\)`$")

omitvars <-  c("^(Intercept)$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^unemploy_diff_d$",
               "^chinesepop_diff_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")

title = "Chinese GFDI and Threat Perceptions, Excluding Most Common Industries"
savename = "results/tablea12.txt"
dep.var="China Threat == 1"
label="tb:appindustry"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("Individual-level Controls",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("District-level Controls",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")))

createTable(results,var.order,savename,dep.var,addto,title)


# Table A13 - Timing of the Announcements
time1a <- felm(chinaect~ nationalism+ education + 
                 econoutlook +  party + age + gender + race + 
                unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
              | sample_state+year| (chinanoiv_d_log_8mo ~ z_log_8mo) |sample_state, # FEs | IVs | Clustered SEs
              data = anesdistricttime2)

time1b <- felm(chinaect~ nationalism+ education + 
                econoutlook + party + age + gender + race + 
                unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
              | sample_state+year| (chinavaliv_d_log_8mo ~ zval_log_8mo) |sample_state, # FEs | IVs | Clustered SEs
              data = anesdistricttime2)

time1c <- felm(chinaect~ nationalism+ education + 
                  econoutlook + party + age + gender + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinajobsiv_d_log_8mo ~ zjobs_log_8mo) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

time2a <- felm(chinaect ~ nationalism+ education + 
                econoutlook + party + age + gender  + race + 
                unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
              | sample_state+year| (chinanoiv_d_log_1y ~ z_log_1y) |sample_state, # FEs | IVs | Clustered SEs
              data = anesdistricttime2)

time2b <- felm(chinaect ~ nationalism+ education + 
                  econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinavaliv_d_log_1y ~ zval_log_1y) |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

time2c <- felm(chinaect ~ nationalism+ education + 
                econoutlook + party + age + gender  + race + 
                unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
              | sample_state+year| (chinajobsiv_d_log_1y ~ zjobs_log_1y) |sample_state, # FEs | IVs | Clustered SEs
              data = anesdistricttime2)

time3a <- felm(chinaect ~ nationalism+ education + 
                 econoutlook + party + age + gender  + race + 
                  unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                | sample_state+year| (chinanoiv_d_log_3y ~ z_log_3y)  |sample_state, # FEs | IVs | Clustered SEs
                data = anesdistricttime2)

time3b <- felm(chinaect ~ nationalism+ education + 
                econoutlook + party + age + gender  + race + 
                unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
              | sample_state+year| (chinavaliv_d_log_3y ~ zval_log_3y)  |sample_state, # FEs | IVs | Clustered SEs
              data = anesdistricttime2)

time3c <- felm(chinaect ~ nationalism+ education + 
                econoutlook + party + age + gender  + race + 
                unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
              | sample_state+year| (chinajobsiv_d_log_3y ~ zjobs_log_3y)  |sample_state, # FEs | IVs | Clustered SEs
              data = anesdistricttime2)

results <- list(time1a,time1b,time1c,time2a,time2b,time2c,time3a,time3b,time3c)

var.order <- c("^`chinanoiv_d_log_8mo\\(fit\\)`$",
               "^`chinavaliv_d_log_8mo\\(fit\\)`$",
               "^`chinajobsiv_d_log_8mo\\(fit\\)`$",
               "^`chinanoiv_d_log_1y\\(fit\\)`$",
               "^`chinavaliv_d_log_1y\\(fit\\)`$",
               "^`chinajobsiv_d_log_1y\\(fit\\)`$",
               "^`chinanoiv_d_log_3y\\(fit\\)`$",
               "^`chinavaliv_d_log_3y\\(fit\\)`$",
               "^`chinajobsiv_d_log_3y\\(fit\\)`$")

omitvars <-  c("^(Intercept)$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")

title = "Chinese GFDI and Threat Perceptions, Different Timing"
savename = "results/tablea13.txt"
dep.var="China Threat == 1"
label="tb:apptime"
addto <- list(c("State Fixed Effects",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("Individual-level Controls",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("District-level Controls",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")))

createTable(results,var.order,savename,dep.var,addto,title)


# Table A14 - Continuous DV 

cont1a <- felm(china_sd ~ nationalism+ education + 
                 econoutlook + party + age + gender  + race  +
                   unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                 | sample_state+year| (chinanoiv_d_log ~ z_log) |sample_state, # FEs | IVs | Clustered SEs
                 data = anesdistricttime2)

cont1b <- felm(chinaord ~ nationalism+ education + 
                 econoutlook + party + age + gender  + race + 
                     unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                   | sample_state| (chinanoiv_d_log ~ z_log)  |sample_state, # FEs | IVs | Clustered SEs
                   data = anesdistricttime2,subset=year==2012)

cont1c <- felm(chinaord ~ nationalism + education + 
                 econoutlook +  party + age + gender + race + 
                     unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                   | sample_state| (chinanoiv_d_log ~ z_log)  |sample_state, # FEs | IVs | Clustered SEs
                   data = anesdistricttime2,subset=year==2020)

cont2a <- felm(china_sd ~ nationalism+ education + 
                 econoutlook + party + age + gender  + race +
                 unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
               | sample_state+year| (chinavaliv_d_log ~ zval_log) |sample_state, # FEs | IVs | Clustered SEs
               data = anesdistricttime2)

cont2b <- felm(chinaord ~ nationalism+ education + 
                 econoutlook + party + age + gender  + race + 
                 unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
               | sample_state| (chinavaliv_d_log ~ zval_log)  |sample_state, # FEs | IVs | Clustered SEs
               data = anesdistricttime2,subset=year==2012)

cont2c <- felm(chinaord ~ nationalism+ education + 
                 econoutlook + party + age + gender  + race + 
                 unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
               | sample_state| (chinavaliv_d_log ~ zval_log)  |sample_state, # FEs | IVs | Clustered SEs
               data = anesdistricttime2,subset=year==2020)

cont3a <- felm(china_sd ~ nationalism+ education + 
                 econoutlook + party + age + gender  + race +
                   unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                 | sample_state+year| (chinajobsiv_d_log ~ zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                 data = anesdistricttime2)

cont3b <- felm(chinaord ~ nationalism+ education + 
                     econoutlook + party + age + gender + race + 
                     unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                   | sample_state| (chinajobsiv_d_log ~ zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                   data = anesdistricttime2,subset=year==2012)

cont3c <- felm(chinaord ~ nationalism + education + 
                     econoutlook + party + age + gender  + race  +
                     unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
                   | sample_state| (chinajobsiv_d_log ~ zjobs_log)  |sample_state, # FEs | IVs | Clustered SEs
                   data = anesdistricttime2,subset=year==2020)


results <- list(cont1a,cont1b,cont1c,cont2a,cont2b,cont2c,cont3a,cont3b,cont3c)

var.order <- c("^`chinanoiv_d_log\\(fit\\)`$",
               "^`chinavaliv_d_log\\(fit\\)`$",
               "^`chinajobsiv_d_log\\(fit\\)`$")

omitvars <-  c("^(Intercept)$",
               "^nationalism$",
               "^education$",
               "^econoutlook$",
               "^party$",
               "^age$",
               "^gender$",
               "^unemploy_ivbase_d$",
               "^per_manu_ivbase_d$",
               "^perchinese_ivbase_d$",
               "^medinc_ivbase_d_log$")

savename = "results/tablea14.txt"
dep.var=c("\\shortstack{China Threat \\\\ Rescaled}",
          "\\shortstack{China Threat \\\\ Cont. 2012}", 
          "\\shortstack{China Threat \\\\ Cont 2020}",
          "\\shortstack{China Threat \\\\ Rescaled}",
          "\\shortstack{China Threat \\\\ Cont. 2012}", 
          "\\shortstack{China Threat \\\\ Cont 2020}",
          "\\shortstack{China Threat \\\\ Rescaled}",
          "\\shortstack{China Threat \\\\ Cont. 2012}", 
          "\\shortstack{China Threat \\\\ Cont 2020}")


title= "Alternative Codings of the China Threat Dependent Variable"
label="tb:appdv"
addto <- list(c("Individual-Level Controls",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("District-Level Controls",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("State Fixed Effects",
                c("Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
              c("Year Fixed Effects",
                c("Yes","No","No","Yes","No","No","Yes","No","No")))

var.label <- str_replace_all(var.order, "\\^", "")
var.label <- str_replace_all(var.label, "\\$", "")

sink(savename)
stargazer(results,
          #p = pvalue.main, 
          p.auto = FALSE,
          t.auto = FALSE,
          ci = FALSE,
          report = ("vc*s"),
          digits = 3, 
          type = "latex",
          #type = "text",
          title =  title,
          #dep.var.labels = "",
          model.names = FALSE,
          column.labels = dep.var,
          order = var.order,
          omit=c(omitvars,"Constant"),
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df),
          label = label,
          omit.stat=c("ll","aic","bic","ser","adj.rsq","rsq"),
          add.lines = addto,
          single.row = FALSE,
          df = FALSE,
          font.size = "normalsize",
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("*", "**", "***"),
          #notes = c("Robust standard errors clustered by ZIP code in parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          #notes = c("95\\% confidence intervals and p-values are presented. Calculations are based on robust standard errors clustered by ZIP code."),
          notes.align = "l",
          notes.append = FALSE,
          no.space=TRUE,
          dep.var.caption = "",
          dep.var.labels.include = FALSE)
sink()

createTable(results,var.order,savename,dep.var,addto,title)

# Table A15 - Outliers 

# 2 standard deviation above the mean of jobs created - 39.38 + 2*154.1003
anesoutlierjobs <- anesdistricttime2 %>%
  filter(ch_ctyjobs_d<348)

write.dta(anesoutlierjobs,file="anesoutlierjobs.dta")

out1 <- felm(chinaect~ nationalism+ education + 
               econoutlook + party + age + gender + race +
                 unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
               | sample_state+year| (chinajobsiv_d_log ~ zjobs_log) |sample_state, # FEs | IVs | Clustered SEs
               data = anesoutlierjobs)

# 2 standard deviation above the mean of investment value - 17.181 + 2 * 99.75901
anesoutlierval <- anesdistricttime2 %>% 
  filter(ch_ctyval_d<217)

write.dta(anesoutlierval,file="anesoutlierval.dta")


out2 <- felm(chinaect~ nationalism+ education + 
               econoutlook +  party + age + gender + race +
               unemploy_ivbase_d + per_manu_ivbase_d + perchinese_ivbase_d + medinc_ivbase_d_log
             | sample_state+year| (chinavaliv_d_log ~ zval_log) |sample_state, # FEs | IVs | Clustered SEs
             data = anesoutlierval)

             
             
