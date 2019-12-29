#------------------------------------------------------------------
# Title: NHATS sample data cleaning 
# Author: Eleanor Hayes-Larson
#------------------------------------------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2")

#------------------------------------------------------------------
# Load data and examine contents #
#------------------------------------------------------------------
raw_data<-read_sas("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis.sas7bdat")


