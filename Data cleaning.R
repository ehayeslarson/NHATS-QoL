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
    #Dementia status
        table(raw_data$r5demclas,exclude=NULL)  #No missing dementia classifications (Taylor dropped nursing home residents and others without SP questionnaire)
        table(raw_data$r5demclas2,exclude=NULL) #This is same variable after Taylor reclassified missing FQs as missing. Identical to previous variable because we dropped all these people anway.
        
        clean_data$dementia.status<-raw_data$r5demclas #Store dementia classification in clean dataset
        clean_data$dementia.bin<-ifelse(clean_data$dementia.status==1 | clean_data$dementia.status==2,1,0) #Create binary dementia variable in clean dataset
                  table(clean_data$dementia.bin,clean_data$dementia.status,exclude=NULL) #Check creation of binary variable
        #FINAL CODING# clean_data$dementia.status: 1=probably, 2=possible, 3=dementia. 
        #FINAL CODING# clean_data$dementia.bin: 1=probable/possible dementia, 0=no dementia
                  
      #Proxy reporting
          table(raw_data$is5resptype, exclude=NULL) #check respondent type
          clean_data$proxy<-ifelse(raw_data$is5resptype==2,1,0) #create clean indicator for proxy respondent
            table(clean_data$proxy,raw_data$is5resptype) #Check recoding              
    #Weights
        summary(raw_data$w5anfinwgt0, exclude=NULL) #check weights, all non-zero weights
        clean_data$w5anfinwgt0<-raw_data$w5anfinwgt0
        
        table(raw_data$w5varstrat, exclude=NULL)#check strata variable
        clean_data$w5varstrat<-raw_data$w5varstrat
        
        table(raw_data$w5varunit, exclude=NULL) #check cluster variable
        clean_data$w5varunit<-raw_data$w5varunit
          
#Cleaning exposure variable 
    table(raw_data$rl5dracehisp,exclude=NULL) 
    
    clean_data$race.eth<-ifelse(raw_data$rl5dracehisp==1 | raw_data$rl5dracehisp==2 ,raw_data$rl5dracehisp, #If white or black, clean is same as raw
                                  ifelse(raw_data$rl5dracehisp==4, 3, #if hispanic, code as "3"
                                          ifelse(raw_data$rl5dracehisp==3,4,NA))) #if other, code as "4", else if multiple no primary (n=8) or no response, code as missing (n=179)
                                  
        table(clean_data$race.eth, raw_data$rl5dracehisp,exclude=NULL)#Check recoding work. 
    #FINAL CODING# clean_data$race.eth: 1=white, 2=black, 3=hispanic, 4=other, NA=missing/multiple no primary
                              

#Cleaning and deriving outcome variables 
    #Schwartz et al. 2019 5 indicators mapping onto SF-12 domains
          #1. Probable depression
                table(raw_data$hc5depresan1, exclude=NULL)
                    temp_depresan1<-ifelse(raw_data$hc5depresan1==-7|raw_data$hc5depresan1==-8,NA,raw_data$hc5depresan1-1) #recode 0-3 instead of 1-4
                    table(temp_depresan1,raw_data$hc5depresan1,exclude=NULL) #check temp variable 
                    
                table(raw_data$hc5depresan2, exclude=NULL)
                    temp_depresan2<-ifelse(raw_data$hc5depresan2==-7|raw_data$hc5depresan2==-8,NA,raw_data$hc5depresan2-1) #recode 0-3 instead of 1-4
                    table(temp_depresan2,raw_data$hc5depresan2,exclude=NULL) #check temp variable 
                
                temp_dep.sum<-temp_depresan1+temp_depresan2 
                temp_prob.dep<-(temp_dep.sum>=3 | temp_depresan1>=3 | temp_depresan2>=3) #create temp depression variable
                    table(temp_prob.dep, temp_depresan1, temp_depresan2,exclude=NULL) #check coding of temp depression variable
                
                clean_data$prob.dep<-ifelse(temp_prob.dep,1, ifelse(!temp_prob.dep,0,NA)) #code clean depression indicator outcome    
                    table(clean_data$prob.dep, temp_prob.dep,exclude=NULL) #Check coding
                #FINAL CODING# clean_data$prob.dep: 1=probable depression (>=3 on PQ2), 0=no probable depression (0-2 on PQ2)
                    
                    
          #2. Probable anxiety
                table(raw_data$hc5depresan3, exclude=NULL)
                temp_depresan3<-ifelse(raw_data$hc5depresan3==-7|raw_data$hc5depresan3==-8,NA,raw_data$hc5depresan3-1) #recode 0-3 instead of 1-4
                    table(temp_depresan3,raw_data$hc5depresan3,exclude=NULL) #check temp variable 
                    
                table(raw_data$hc5depresan4, exclude=NULL)
                temp_depresan4<-ifelse(raw_data$hc5depresan4==-7|raw_data$hc5depresan4==-8,NA,raw_data$hc5depresan4-1) #recode 0-3 instead of 1-4
                    table(temp_depresan4,raw_data$hc5depresan4,exclude=NULL) #check temp variable 
                    
                temp_anx.sum<-temp_depresan3+temp_depresan4 
                temp_prob.anx<-(temp_anx.sum>=3 | temp_depresan3>=3 | temp_depresan4>=3) #create temp depression variable
                    table(temp_prob.anx, temp_depresan3, temp_depresan4,exclude=NULL) #check coding of temp depression variable
                    
                clean_data$prob.anx<-ifelse(temp_prob.anx,1, ifelse(!temp_prob.anx,0,NA)) #code clean depression indicator outcome    
                    table(clean_data$prob.anx, temp_prob.anx,exclude=NULL) #Check coding
                #FINAL CODING# clean_data$prob.anx: 1=probable anxiety (>=3 on GAD2), 0=no probable anxiety (0-2 on GAD2)
                    
                    
          #3. Self-reported health
                table(raw_data$hc5health, exclude=NULL) #examine raw data
                temp_poorhealth<-ifelse(raw_data$hc5health==-7|raw_data$hc5health==-8,NA,raw_data$hc5health)
                temp_poorhealth.bin<-(temp_poorhealth>=4)
                table(temp_poorhealth.bin, exclude=NULL)
                
                clean_data$poorhealth.bin<-ifelse(temp_poorhealth.bin,1, ifelse(!temp_poorhealth.bin,0,NA)) #code clean depression indicator outcome    
                    table(clean_data$poorhealth.bin, raw_data$hc5health, exclude=NULL) #check coding
                #FINAL CODING# clean_data$poorhealth.bin: 1=fair/poor self-rated health, 0=excellent, very good, or good self-rated health
                    
          #4. Bothered by pain in last month
                table(raw_data$ss5painbothr, exclude=NULL)
                
                clean_data$pain.bother[raw_data$ss5painbothr==1]<-1 #Recode to painbother = 1 if yes
                clean_data$pain.bother[raw_data$ss5painbothr==2]<-0 #Recode to painbother = 1 if no
                clean_data$pain.bother[raw_data$ss5painbothr==-8]<-NA #Recode to painbother = NA if ref/dk
                  table(clean_data$pain.bother, raw_data$ss5painbothr, exclude=NULL) #Check recoding
                #FINAL CODING# clean_data$pain.bother: 1=yes, 2=no.
          
          #5. Functional limitations 
                table(raw_data$mo5dbedhelp, exclude=NULL)
                temp_iadl1<-ifelse(raw_data$mo5dbedhelp==1,0,ifelse(raw_data$mo5dbedhelp==2,1,NA)) #recode to 1/0/NA
                  table(temp_iadl1, raw_data$mo5dbedhelp, exclude=NULL) #check coding 
                
                table(raw_data$mo5dinsdhelp, exclude=NULL)
                temp_iadl2<-ifelse(raw_data$mo5dinsdhelp==1,0,ifelse(raw_data$mo5dinsdhelp==2,1,NA)) #recode to 1/0/NA
                  table(temp_iadl2, raw_data$mo5dinsdhelp, exclude=NULL) #check coding 
                  
                table(raw_data$sc5deathelp, exclude=NULL)
                temp_iadl3<-ifelse(raw_data$sc5deathelp==1,0,ifelse(raw_data$sc5deathelp==2,1,NA)) #recode to 1/0/NA
                  table(temp_iadl3, raw_data$sc5deathelp, exclude=NULL) #check coding 
                  
                table(raw_data$sc5dbathhelp, exclude=NULL)
                temp_iadl4<-ifelse(raw_data$sc5dbathhelp==1,0,ifelse(raw_data$sc5dbathhelp==2,1,NA)) #recode to 1/0/NA
                  table(temp_iadl4, raw_data$sc5dbathhelp, exclude=NULL) #check coding 
                  
                table(raw_data$sc5dtoilhelp, exclude=NULL)
                temp_iadl5<-ifelse(raw_data$sc5dtoilhelp==1,0,ifelse(raw_data$sc5dtoilhelp==2,1,NA)) #recode to 1/0/NA
                  table(temp_iadl5, raw_data$sc5dtoilhelp, exclude=NULL) #check coding 
                  
                table(raw_data$sc5ddreshelp, exclude=NULL)
                temp_iadl6<-ifelse(raw_data$sc5ddreshelp==1,0,ifelse(raw_data$sc5ddreshelp==2,1,NA)) #recode to 1/0/NA
                  table(temp_iadl6, raw_data$sc5ddreshelp, exclude=NULL) #check coding 
                  
                temp_iadl.max<-pmax(temp_iadl1, temp_iadl2, temp_iadl3, temp_iadl4, temp_iadl5, temp_iadl6)
                table(temp_iadl.max, exclude=NULL)
                
                temp_funclimits<-(temp_iadl.max==1 | temp_iadl1==1 | temp_iadl2==1 | temp_iadl3==1 | temp_iadl4==1 | temp_iadl5==1 | temp_iadl6==1)
                  table(temp_funclimits, temp_iadl.max, exclude=NULL)
                
                clean_data$funclimits<-ifelse(temp_funclimits,1, ifelse(!temp_funclimits,0,NA)) #code clean functional limitations indicator outcome    
                  table(clean_data$funclimits, temp_funclimits,exclude=NULL) #Check coding
                #FINAL CODING# data_clean$funclimits: 1=help with >=1 IADL in last month, 0=no help with any IADLs in last month
                  
    #Kasper et al. 2018/Freedman et al. 2014 Six item score from 0-20
                table(raw_data$wb5offelche1, exclude=NULL)
                temp_feel1<-abs(raw_data$wb5offelche1-5) #reverse code from 0-5
                temp_feel1<-ifelse(temp_feel1>5,NA,temp_feel1) #make proxy repsondents (N=442), ref/dk = missing
                  table(temp_feel1,raw_data$wb5offelche1, exclude=NULL) #check work
                
  
                table(raw_data$wb5offelche2, exclude=NULL)
                temp_feel2<-abs(raw_data$wb5offelche2-5) #reverse code from 0-5
                temp_feel2<-ifelse(temp_feel2>5,NA,temp_feel2) #make proxy repsondents (N=442), ref/dk = missing
                  table(temp_feel2,raw_data$wb5offelche2, exclude=NULL) #check work
                
                table(raw_data$wb5offelche3, exclude=NULL)
                temp_feel3<-abs(raw_data$wb5offelche3-5) #reverse code from 0-5
                temp_feel3<-ifelse(temp_feel3>5,NA,temp_feel3) #make proxy repsondents (N=442), ref/dk = missing
                  table(temp_feel3,raw_data$wb5offelche3, exclude=NULL) #check work
                
                table(raw_data$wb5offelche4, exclude=NULL)
                temp_feel4<-abs(raw_data$wb5offelche4-5) #reverse code from 0-5
                temp_feel4<-ifelse(temp_feel4>5,NA,temp_feel4) #make proxy repsondents (N=442), ref/dk = missing
                  table(temp_feel4,raw_data$wb5offelche4, exclude=NULL) #check work
                
  
                table(raw_data$wb5truestme1, exclude=NULL)
                temp_feel5<-abs(raw_data$wb5truestme1-3) #reverse code from 0-5
                temp_feel5<-ifelse(temp_feel5>2,NA,temp_feel5) #make proxy repsondents (N=442), ref/dk = missing
                  table(temp_feel5,raw_data$wb5truestme1, exclude=NULL) #check work
                  
                table(raw_data$wb5truestme2, exclude=NULL)
                temp_feel6<-abs(raw_data$wb5truestme2-3) #reverse code from 0-5
                temp_feel6<-ifelse(temp_feel6>2,NA,temp_feel6) #make proxy repsondents (N=442), ref/dk = missing
                  table(temp_feel6,raw_data$wb5truestme2, exclude=NULL) #check work
                  
              temp_wbscore<-temp_feel1+temp_feel2+temp_feel3+temp_feel4+temp_feel5+temp_feel6 #add up scale items, missing sum if missing any                    
              table(temp_wbscore,exclude=NULL)  
              temp_wbscoremiss<-ifelse(is.na(temp_wbscore),1,0)
              hist(temp_wbscore)
              
              clean_data$wbscore<-temp_wbscore #save coded variable
              #FINAL CODING# clean_data$wbscore: continuous var, missing if any contributing item missing. 
              
              table(clean_data$race.eth,temp_wbscoremiss,clean_data$dementia.bin,exclude=NULL)

#Cleaning covariates
    #Age
      table(raw_data$r5d2intvrage, exclude=NULL) #No missing, "inapplicable" or weird values
    
      clean_data$age.cat<-raw_data$r5d2intvrage #copy variables to clean dataframe
      #FINAL CODING# clean_data$age.cat: 1=65-69, 2=70-74, 3=75-79, 4=80-84, 5=85-89, 6=90+


    #Gender
      table(raw_data$r5dgender, exclude=NULL) #No missing or weird gender
      
      clean_data$female[raw_data$r5dgender==1]<-0 #Recode to female = 0 if male
      clean_data$female[raw_data$r5dgender==2]<-1 #Recode to female = 1 if female
      
      table(clean_data$female, raw_data$r5dgender,exclude=NULL) #Check recoding
      #FINAL CODING# clean_data$female: 1=female, 2=male

      
    clean_data$comp.case.HRQoL<-complete.cases(clean_data[1:13])
    clean_data$comp.case.WBQoL<-complete.cases(clean_data[c(1:8,14)])
    clean_data$comp.case.all<-complete.cases(clean_data)
    

    #------------------------------------------------------------------
    # Save clean data and remove temporary objects
    #------------------------------------------------------------------
    save(clean_data, file="C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean.RData")

    rm(list=ls(pattern="temp"))
      