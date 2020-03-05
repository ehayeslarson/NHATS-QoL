#------------------------------------------------------------------
# Title: HRQoL descriptive analysis pooled
# Author: Eleanor Hayes-Larson
#------------------------------------------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "gee", "wgeesel", 
       "survey", "tableone", "openxlsx")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")
clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]
clean_data_hrqol_dem<-clean_data[clean_data$comp.case.HRQoL==1 & clean_data$dementia.bin==1,]
clean_data_hrqol_nodem<-clean_data[clean_data$comp.case.HRQoL==1 & clean_data$dementia.bin==0,]


# ---Unweighted analyses--- #

#Sample descriptives

catvars<-c("age.cat", "female", "race.eth", "prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")
allvars<-c(catvars, )
CreateTableOne(vars=catvars, data=clean_data_hrqol_dem, factorVars=catvars)

#Check differences in proxy by race
CreateTableOne(vars="proxy", strata="race.eth", data=clean_data_hrqol_dem, factorVars=c("race.eth","proxy"))


#Check differences in proxy by race
CreateTableOne(vars="proxy", strata="race.eth", data=clean_data_hrqol, factorVars=c("race.eth","proxy"))




# ---Weighted analyses--- #



#weighted
svymean(~female, nhats_design_nodem)
svymean(~as.factor(race.eth), nhats_design_nodem)
svymean(~as.factor(age.cat), nhats_design_nodem)
svymean(~prob.dep, nhats_design_nodem)
svymean(~prob.anx, nhats_design_nodem)
svymean(~poorhealth.bin, nhats_design_nodem)
svymean(~pain.bother, nhats_design_nodem)
svymean(~funclimits, nhats_design_nodem)
