#---------------------------------
# Cleaning pooled data
#---------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2")

#------------------------------------------------------------------
# Load data and do data cleaning #
#------------------------------------------------------------------
raw_data<-read_sas("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/nhats_qoldem_pooled.sas7bdat")

