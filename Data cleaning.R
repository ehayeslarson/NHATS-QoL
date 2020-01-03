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
# Load data and do data cleaning #
#------------------------------------------------------------------
raw_data<-read_sas("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis.sas7bdat")

clean_data<-data.frame(spid=raw_data$spid) #Create new dataset to store cleaned variables


#Cleaning selection variable (derived dementia variable)
    table(raw_data$r5demclas)
    table(raw_data$r5demclas2)
    summary(is.na(raw_data$r5demclas)) #No missing dementia classifications (Taylor dropped nursing home residents and others without SP questionnaire)
    
    
    clean_data$dementia.status<-raw_data$r5demclas #Store dementia classification in clean dataset
    clean_data$dementia.bin<-ifelse(clean_data$dementia.status==1 | clean_data$dementia.status==2,1,0) #Create binary dementia variable in clean dataset
              table(clean_data$dementia.bin,clean_data$dementia.status,exclude=NULL) #Check creation of binary variable
    #FINAL CODING# clean_data$dementia.status: 1=probably, 2=possible, 3=dementia. 
    #FINAL CODING# clean_data$dementia.bin: 1=probable/possible dementia, 0=no dementia
          
#Cleaning exposure variable 
    table(raw_data$rl5dracehisp) 
    
    clean_data$race.eth<-ifelse(raw_data$rl5dracehisp==1 | raw_data$rl5dracehisp==2 ,raw_data$rl5dracehisp, #If white or black, clean is same as raw
                                  ifelse(raw_data$rl5dracehisp==4, 3, #if hispanic, code as "3"
                                          ifelse(raw_data$rl5dracehisp==3,4,NA))) #if other, code as "4", else if multiple no primary (n=8) or no response, code as missing (n=179)
                                  
        table(clean_data$race.eth, raw_data$rl5dracehisp,exclude=NULL)#Check recoding work. 
    #FINAL CODING# clean_data$race.eth: 1=white, 2=black, 3=hispanic, 4=other, NA=missing/multiple no primary
                              

#Cleaning and deriving outcome variables 


#Cleaning covariates
summary(is.na(raw_data$r5d2intvrage)) #No missing age
summary(is.na(raw_data$r5dgender)) #No missing gender


data