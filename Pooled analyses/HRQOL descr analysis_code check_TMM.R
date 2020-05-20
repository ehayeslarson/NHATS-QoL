#------------------------------------------------------------------
# Title: HRQoL descriptive analysis pooled -- code check by TMM
# Author: Taylor Mobley
#------------------------------------------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "gee", "wgeesel", 
       "survey", "tableone", "openxlsx", "rlang")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
#load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")

load("C:/Users/tmobley/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")

clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

#Keep baseline observation for participants in clean_data_hrqol
idlist<-clean_data_hrqol %>% distinct(spid)
idlist<-idlist[,1]
clean_data_hrqol_baseline<-clean_data %>% filter(.,first.obs==1, spid %in% idlist)

#------------------------------------------------------------------
# ---Unweighted analyses--- #
#------------------------------------------------------------------

#Race/ethnicity variable check - should not have "other"
table(clean_data_hrqol_baseline$race.eth, exclude=NULL)

#Unweighted freqs and percentages
table1.age<-table(clean_data_hrqol_baseline$age.cat, clean_data_hrqol_baseline$race.eth)
table1.age
round(100*prop.table(table1.age,2),digits=1)

table1.sex<-table(clean_data_hrqol_baseline$female, clean_data_hrqol_baseline$race.eth)
table1.sex
round(100*prop.table(table1.sex,2),digits=1)

table1.edu<-table(clean_data_hrqol_baseline$edu.7cat, clean_data_hrqol_baseline$race.eth)
table1.edu
round(100*prop.table(table1.edu,2),digits=1)

table1.born<-table(clean_data_hrqol_baseline$born.us, clean_data_hrqol_baseline$race.eth)
table1.born
round(100*prop.table(table1.born,2),digits=1)

table1.resid<-table(clean_data_hrqol_baseline$resid.care, clean_data_hrqol_baseline$race.eth)
table1.resid
round(100*prop.table(table1.resid,2),digits=1)

table1.census<-table(clean_data_hrqol_baseline$cens.area, clean_data_hrqol_baseline$race.eth)
table1.census
round(100*prop.table(table1.census,2),digits=1)

table1.proxy<-table(clean_data_hrqol_baseline$proxy, clean_data_hrqol_baseline$race.eth)
table1.proxy
round(100*prop.table(table1.proxy,2),digits=1)

table1.proxyfam<-table(clean_data_hrqol_baseline$proxy.fam, clean_data_hrqol_baseline$race.eth)
table1.proxyfam
round(100*prop.table(table1.proxyfam,2),digits=1)

table1.highbp<-table(clean_data_hrqol_baseline$sr.highbp, clean_data_hrqol_baseline$race.eth)
table1.highbp
round(100*prop.table(table1.highbp,2),digits=1)

table1.dm<-table(clean_data_hrqol_baseline$sr.diabetes, clean_data_hrqol_baseline$race.eth)
table1.dm
round(100*prop.table(table1.dm,2),digits=1)

table1.stroke<-table(clean_data_hrqol_baseline$sr.stroke, clean_data_hrqol_baseline$race.eth)
table1.stroke
round(100*prop.table(table1.stroke,2),digits=1)

table1.dem<-table(clean_data_hrqol_baseline$dementia.status, clean_data_hrqol_baseline$race.eth)
table1.dem
round(100*prop.table(table1.dem,2),digits=1)


#------------------------------------------------------------------
# ---Unweighted analyses--- #
#------------------------------------------------------------------
nhats_design<-svydesign(data=clean_data_hrqol_baseline, id=~cluster, strata=~stratum, weights=~analytic.wgt, nest=T)

raceth.wgt<-svytable(~race.eth, nhats_design, exclude=NULL, round=T, na.action=na.pass, addNA=TRUE)
raceth.wgt
100*(50025869/(50025869+5110861+4479530))
100*(5110861/(50025869+5110861+4479530))
100*(4479530/(50025869+5110861+4479530))

age.wgt<-svytable(~age.cat+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(age.wgt,2),digits=1)

sex.wgt<-svytable(~female+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(sex.wgt,2),digits=1)

edu.wgt<-svytable(~edu.7cat+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(edu.wgt,2),digits=1)

born.wgt<-svytable(~born.us+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(born.wgt,2),digits=1)

resid.wgt<-svytable(~resid.care+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(resid.wgt,2),digits=1)

proxy.wgt<-svytable(~proxy+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(proxy.wgt,2),digits=1)

proxyfam.wgt<-svytable(~proxy.fam+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(proxyfam.wgt,2),digits=1)

census.wgt<-svytable(~cens.area+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(census.wgt,2),digits=1)

highbp.wgt<-svytable(~sr.highbp+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(highbp.wgt,2),digits=1)

dm.wgt<-svytable(~sr.diabetes+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(dm.wgt,2),digits=1)

stroke.wgt<-svytable(~sr.stroke+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(stroke.wgt,2),digits=1)

dem.wgt<-svytable(~dementia.status+race.eth, nhats_design, round=T, na.action=na.pass, addNA=FALSE)
round(100*prop.table(dem.wgt,2),digits=1)
