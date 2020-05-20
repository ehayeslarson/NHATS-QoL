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