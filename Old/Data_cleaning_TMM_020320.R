#------------------------------------------------------------------
# Title: NHATS sample data cleaning 
# Author: Taylor Mobley; code adapted from Eleanor Hayes-Larson
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
raw_data<-read_sas("C:/Users/tmobley/Box/NHATS/DATA/analysis_datasets/nhats_qoldem_clean.sas7bdat")

clean_data<-data.frame(spid=raw_data$spid) #Create new dataset to store cleaned variables

#Bringing in observation indicator variables
clean_data$round<-raw_data$round 

clean_data$first.obs<-raw_data$first_obs
clean_data$count.obs<-raw_data$count_obs
clean_data$last.obs<-raw_data$last_obs

#Cleaning categorical age variables
table(raw_data$intvrage,exclude=NULL)
table(raw_data$bl_agecat,exclude=NULL)

  clean_data$age.cat<-raw_data$intvrage
  clean_data$age.cat <- ordered(clean_data$age.cat,
                     levels = c(1,2,3,4,5,6),
                     labels = c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90+")) 
  table(clean_data$age.cat, raw_data$intvrage, exclude=NULL)


  clean_data$baseline.age<-raw_data$bl_agecat
  clean_data$baseline.age <- ordered(clean_data$baseline.age,
                              levels = c(1,2,3,4,5,6),
                              labels = c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90+")) 
  table(clean_data$baseline.age, raw_data$bl_agecat, exclude=NULL)


#Cleaning selection variable (derived dementia variable)
#Dementia status
table(raw_data$demclas,exclude=NULL)  #No missing dementia classifications (Taylor dropped nursing home residents and others without SP questionnaire)

clean_data$dementia.status<-raw_data$demclas #Store dementia classification in clean dataset
clean_data$dementia.bin<-ifelse(clean_data$dementia.status==1 | clean_data$dementia.status==2,1,0) #Create binary dementia variable in clean dataset
table(clean_data$dementia.bin,clean_data$dementia.status,exclude=NULL) #Check creation of binary variable
#FINAL CODING# clean_data$dementia.status: 1=probably, 2=possible, 3=no dementia. 
#FINAL CODING# clean_data$dementia.bin: 1=probable/possible dementia, 0=no dementia

#Proxy reporting
table(raw_data$resptype, exclude=NULL) #check respondent type
clean_data$proxy<-ifelse(raw_data$resptype==2,1,0) #create clean indicator for proxy respondent
table(clean_data$proxy,raw_data$resptype) #Check recoding clean_data$proxy: 0 = SP, 1 = Proxy                

#Weights -- talk to EHL before adding weight variables 

  #Analytic Weight
  summary(raw_data$anfinwgt0, exclude=NULL) #check weights, all non-zero weights
  clean_data$analytic.wgt<-raw_data$anfinwgt0

  #Baseline analytic weight
  summary(raw_data$bl_anfinwgt0, exclude=NULL)
  clean_data$baseline.anwgt<-raw_data$bl_anfinwgt0 
  
  #average analytic weight
  mean_by_person <- raw_data %>% 
    group_by(spid) %>% 
    summarize(average.anwgt = mean(anfinwgt0), min.anwgt = min(anfinwgt0), max.anwgt = max(anfinwgt0))
  mean_by_person$diff.anwgt <- (mean_by_person$max.anwgt - mean_by_person$min.anwgt)
  clean_data<- merge(clean_data, mean_by_person, by="spid")
  
  summary(clean_data$diff.anwgt)
  summary(clean_data$max.anwgt)
   
  #Stratum variable
  table(raw_data$varstrat, exclude=NULL)#check strata variable
  clean_data$stratum<-raw_data$varstrat

  #Baseline stratum variable
  table(raw_data$bl_varstrat)
  clean_data$baseline.stratum<-raw_data$bl_varstrat

  #Cluster variable
  table(raw_data$varunit, exclude=NULL) #check cluster variable
  clean_data$cluster<-raw_data$varunit
  
  #Baseline cluster variable
  table(raw_data$bl_varunit, exclude=NULL) #check cluster variable
  clean_data$baseline.cluster<-raw_data$bl_varunit

#Cleaning exposure variable. From User guide: At round 5, the variable rl5dracehisp was derived from the Round 1 
#variable for continuing sample and from the Round 5 interview for new sample -- TMM 02.13.20
table(raw_data$bl_racehisp,exclude=NULL)

  #baseline
  clean_data$race.eth<-ifelse(raw_data$bl_racehisp==1 | raw_data$bl_racehisp==2 ,raw_data$bl_racehisp, #If white or black, clean is same as raw
                            ifelse(raw_data$bl_racehisp==4, 3, #if hispanic, code as "3"
                                   ifelse(raw_data$bl_racehisp==3,4,NA))) #if other, code as "4", else if multiple no primary (n=27) or don't know/refused (n=257), code as missing (n=284)

  table(clean_data$race.eth, raw_data$bl_racehisp,exclude=NULL)#Check recoding work. 
  #FINAL CODING# clean_data$race.eth: 1=white, 2=black, 3=hispanic, 4=other, NA=missing/multiple no primary


#Cleaning and deriving outcome variables 
#Schwartz et al. 2019 5 indicators mapping onto SF-12 domains
#1. Probable depression
table(raw_data$depresan1, exclude=NULL)
temp_depresan1<-ifelse(raw_data$depresan1==-7|raw_data$depresan1==-8,NA,raw_data$depresan1-1) #recode 0-3 instead of 1-4
table(temp_depresan1,raw_data$depresan1,exclude=NULL) #check temp variable 

table(raw_data$depresan2, exclude=NULL)
temp_depresan2<-ifelse(raw_data$depresan2==-7|raw_data$depresan2==-8,NA,raw_data$depresan2-1) #recode 0-3 instead of 1-4
table(temp_depresan2,raw_data$depresan2,exclude=NULL) #check temp variable 

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
table(raw_data$health, exclude=NULL) #examine raw data
temp_poorhealth<-ifelse(raw_data$health==-7|raw_data$health==-8,NA,raw_data$health)
temp_poorhealth.bin<-(temp_poorhealth>=4)
table(temp_poorhealth.bin, exclude=NULL)

clean_data$poorhealth.bin<-ifelse(temp_poorhealth.bin,1, ifelse(!temp_poorhealth.bin,0,NA)) #code clean depression indicator outcome    
table(clean_data$poorhealth.bin, raw_data$health, exclude=NULL) #check coding
#FINAL CODING# clean_data$poorhealth.bin: 1=fair/poor self-rated health, 0=excellent, very good, or good self-rated health

#4. Bothered by pain in last month
table(raw_data$painbothr, exclude=NULL)

clean_data$pain.bother[raw_data$painbothr==1]<-1 #Recode to painbother = 1 if yes
clean_data$pain.bother[raw_data$painbothr==2]<-0 #Recode to painbother = 0 if no
clean_data$pain.bother[raw_data$painbothr==-8 | raw_data$painbothr==-7]<-NA #Recode to painbother = NA if ref/dk
table(clean_data$pain.bother, raw_data$painbothr, exclude=NULL) #Check recoding
#FINAL CODING# clean_data$pain.bother: 1=yes, 0=no.

#5. Functional limitations 
table(raw_data$bedhelp, exclude=NULL)
temp_iadl1<-ifelse(raw_data$bedhelp==1,0,ifelse(raw_data$bedhelp==2,1,NA)) #recode to 1/0/NA
table(temp_iadl1, raw_data$bedhelp, exclude=NULL) #check coding 

table(raw_data$insdhelp, exclude=NULL)
temp_iadl2<-ifelse(raw_data$insdhelp==1,0,ifelse(raw_data$insdhelp==2,1,NA)) #recode to 1/0/NA
table(temp_iadl2, raw_data$insdhelp, exclude=NULL) #check coding 

table(raw_data$eathelp, exclude=NULL)
temp_iadl3<-ifelse(raw_data$eathelp==1,0,ifelse(raw_data$eathelp==2,1,NA)) #recode to 1/0/NA
table(temp_iadl3, raw_data$eathelp, exclude=NULL) #check coding 

table(raw_data$bathhelp, exclude=NULL)
temp_iadl4<-ifelse(raw_data$bathhelp==1,0,ifelse(raw_data$bathhelp==2,1,NA)) #recode to 1/0/NA
table(temp_iadl4, raw_data$bathhelp, exclude=NULL) #check coding 

table(raw_data$toilhelp, exclude=NULL)
temp_iadl5<-ifelse(raw_data$toilhelp==1,0,ifelse(raw_data$toilhelp==2,1,NA)) #recode to 1/0/NA
table(temp_iadl5, raw_data$toilhelp, exclude=NULL) #check coding 

table(raw_data$dreshelp, exclude=NULL)
temp_iadl6<-ifelse(raw_data$dreshelp==1,0,ifelse(raw_data$dreshelp==2,1,NA)) #recode to 1/0/NA
table(temp_iadl6, raw_data$dreshelp, exclude=NULL) #check coding 

temp_iadl.max<-pmax(temp_iadl1, temp_iadl2, temp_iadl3, temp_iadl4, temp_iadl5, temp_iadl6)
table(temp_iadl.max, exclude=NULL)

temp_funclimits<-(temp_iadl.max==1 | temp_iadl1==1 | temp_iadl2==1 | temp_iadl3==1 | temp_iadl4==1 | temp_iadl5==1 | temp_iadl6==1)
table(temp_funclimits, temp_iadl.max, exclude=NULL)

clean_data$funclimits<-ifelse(temp_funclimits,1, ifelse(!temp_funclimits,0,NA)) #code clean functional limitations indicator outcome    
table(clean_data$funclimits, temp_funclimits,exclude=NULL) #Check coding
#FINAL CODING# data_clean$funclimits: 1=help with >=1 IADL in last month, 0=no help with any IADLs in last month

#Kasper et al. 2018/Freedman et al. 2014 Six item score from 0-20
table(raw_data$offelche1, exclude=NULL)
temp_feel1<-abs(raw_data$offelche1-5) #reverse code from 0-5
temp_feel1<-ifelse(temp_feel1>5,NA,temp_feel1) #make proxy repsondents (N=442), ref/dk = missing
table(temp_feel1,raw_data$offelche1, exclude=NULL) #check work


table(raw_data$offelche2, exclude=NULL)
temp_feel2<-abs(raw_data$offelche2-5) #reverse code from 0-5
temp_feel2<-ifelse(temp_feel2>5,NA,temp_feel2) #make proxy repsondents (N=442), ref/dk = missing
table(temp_feel2,raw_data$offelche2, exclude=NULL) #check work

table(raw_data$offelche3, exclude=NULL)
temp_feel3<-abs(raw_data$offelche3-5) #reverse code from 0-5
temp_feel3<-ifelse(temp_feel3>5,NA,temp_feel3) #make proxy repsondents (N=442), ref/dk = missing
table(temp_feel3,raw_data$offelche3, exclude=NULL) #check work

table(raw_data$offelche4, exclude=NULL)
temp_feel4<-abs(raw_data$offelche4-5) #reverse code from 0-5
temp_feel4<-ifelse(temp_feel4>5,NA,temp_feel4) #make proxy repsondents (N=442), ref/dk = missing
table(temp_feel4,raw_data$offelche4, exclude=NULL) #check work


table(raw_data$truestme1, exclude=NULL)
temp_feel5<-abs(raw_data$truestme1-3) #reverse code from 0-5
temp_feel5<-ifelse(temp_feel5>2,NA,temp_feel5) #make proxy repsondents (N=442), ref/dk = missing
table(temp_feel5,raw_data$truestme1, exclude=NULL) #check work

table(raw_data$truestme2, exclude=NULL)
temp_feel6<-abs(raw_data$truestme2-3) #reverse code from 0-5
temp_feel6<-ifelse(temp_feel6>2,NA,temp_feel6) #make proxy repsondents (N=442), ref/dk = missing
table(temp_feel6,raw_data$truestme2, exclude=NULL) #check work

temp_wbscore<-temp_feel1+temp_feel2+temp_feel3+temp_feel4+temp_feel5+temp_feel6 #add up scale items, missing sum if missing any                    
table(temp_wbscore,exclude=NULL)  
temp_wbscoremiss<-ifelse(is.na(temp_wbscore),1,0)
hist(temp_wbscore)

clean_data$wbscore<-temp_wbscore #save coded variable
#FINAL CODING# clean_data$wbscore: continuous var, missing if any contributing item missing. 

table(clean_data$race.eth,temp_wbscoremiss,clean_data$dementia.bin,exclude=NULL)

#Cleaning covariates

#Gender
table(raw_data$bl_gender,exclude=NULL)
clean_data$female<- ifelse(raw_data$bl_gender==2, 1,
                           ifelse(raw_data$bl_gender==1,0,NA))
table(clean_data$female, raw_data$bl_gender,exclude=NULL) # 1=female 2=male

table(clean_data$female, clean_data$race.eth, exclude=NULL)

#Education -- Should we think about collapsing var to missing/<=8 vs >8 like Chloe suggests in KHANDLE?
table(raw_data$bl_higstschl, exclude=NULL)

clean_data$edu.cat<-ifelse(raw_data$bl_higstschl==-7,NA,
                           ifelse(raw_data$bl_higstschl==-8,NA,raw_data$bl_higstschl))

clean_data$edu.cat <- ordered(clean_data$edu.cat,
                              levels = c(1,2,3,4,5,6,7,8,9),
                              labels = c("1: NO SCHOOL COMPLETED", "2: 1ST-8TH GRADE", "3: 9TH-12TH GRADE NO DIPLOMA", 
                                          "4: HIGH SCHOOL GRADUATE HIGH SCHOOL DIPLOMA OR EQUIVALENT",
                                          "5: VOCATIONAL, TECHNICAL, BUSINESS, OR TRADE SCHOOL CERTIFICATE OR DIPLOMA BEYOND HIGH SCHOOL LEVEL",
                                          "6: SOME COLLEGE BUT NO DEGREE",
                                          "7: ASSOCIATES DEGREE",
                                          "8: BACHELORS DEGREE",
                                          "9: MASTERS, PROFESSIONAL, OR DOCTORAL DEGREE"))
table(clean_data$edu.cat,raw_data$bl_higstschl, exclude=NULL)

clean_data$edu.bin [raw_data$bl_higstschl==1 | raw_data$bl_higstschl==2 | raw_data$bl_higstschl==3 | raw_data$bl_higstschl==4]<-0 #No education - HS diploma
clean_data$edu.bin [raw_data$bl_higstschl==5 | raw_data$bl_higstschl==6 | raw_data$bl_higstschl==7 | raw_data$bl_higstschl==8 | raw_data$bl_higstschl==9]<-1 #HS diploma +
clean_data$edu.bin [raw_data$bl_higstschl==-7 | raw_data$bl_higstschl==-8]<-NA
table(clean_data$edu.bin, exclude=NULL)

table(clean_data$edu.cat, clean_data$edu.bin, exclude=NULL)

#Residential status
table(raw_data$resid, exclude=NULL)
clean_data$resid.care<-ifelse(raw_data$resid==2,1,
                              ifelse(raw_data$resid==1,0,NA))
table(clean_data$resid.care, exclude=NULL)
#FINAL CODING: 1 = Community, 2 = Residential care, not NH (with SP interview)  

#Census div
table(raw_data$censdiv, exclude=NULL)
clean_data$cens.div<-raw_data$censdiv
clean_data$cens.div<-ordered(clean_data$cens.div,
                             levels=c(1,2,3,4,5,6,7,8,9),
                             labels=c("1: Northeast Region: New England Division", "2: Northeast Region: Middle Atlantic Division",
                                        "3: Midwest Region: East North Central Division", "4: Midwest Region: West North Central Division",
                                        "5: South Region: South Atlantic Division", "6: South Region: East South Central Division",
                                        "7: South Region: West South Central Division", "8: West Region: Mountain Division", "9: West Region: Pacific Division"))
table(clean_data$cens.div, raw_data$censdiv, exclude=NULL)


#Born in US
table(raw_data$bl_borninus, exclude=NULL)
clean_data$born.us<-ifelse(raw_data$bl_borninus==1,1,
                          ifelse(raw_data$bl_borninus==2,0,NA))
table(clean_data$born.us,exclude=NULL)
table(clean_data$born.us, raw_data$bl_borninus, exclude=NULL)
#FINAL CODING: 1=born in US, 0=not born in us

#Age to US
table(raw_data$bl_age2us, exclude=NULL)
clean_data$age2us<-ifelse(raw_data$bl_age2us==-7,NA,
                           ifelse(raw_data$bl_age2us==-8,NA,
                                  ifelse(raw_data$bl_age2us==-9,NA,
                                         ifelse(raw_data$bl_age2us==997,NA,raw_data$bl_age2us)))) #997 born in US, set to NA
table(clean_data$age2us, exclude=NULL)
table(clean_data$age2us, clean_data$born.us, exclude=NULL)

#Proxy familarity
table(raw_data$famrrutin,exclude=NULL)
clean_data$proxy.fam<-ifelse(raw_data$famrrutin==-1,NA, #Recode range from 1-4 to 0-3
                             ifelse(raw_data$famrrutin==1,3, #Recode very familiar = 3
                                    ifelse(raw_data$famrrutin==2,2, #Somewhat familiar = 2
                                           ifelse(raw_data$famrrutin==3,1, #Recode a little familiar = 1
                                                  ifelse(raw_data$famrrutin==4,0,raw_data$famrrutin))))) #Recode Not at all familiar = 0
table(raw_data$famrrutin, clean_data$proxy.fam, exclude=NULL)
#FINAL CODING: 3 = Very familiar, 2 = Somewhat familiar, 1 = A little familiar, 0 = Not at all familiar

#Proxy lives with SP
table(raw_data$proxlivsp,exclude=NULL)
clean_data$proxy.livsp<-ifelse(raw_data$proxlivsp==1,1,
                               ifelse(raw_data$proxlivsp==2,0,
                                ifelse(raw_data$proxlivsp==-1,NA,NA)))
table(raw_data$proxlivsp, clean_data$proxy.livsp, exclude=NULL)

#Proxy relation to SP
table(raw_data$prxyrelat, exclude=NULL)
clean_data$proxy.relat<-ifelse(raw_data$prxyrelat==-1,NA,raw_data$prxyrelat)
table(raw_data$prxyrelat, clean_data$proxy.relat, exclude=NULL)

#SP living arrangement
table(raw_data$lvngarrg, exclude=NULL)
clean_data$sp.livarrg<-ifelse(raw_data$lvngarrg==-9,NA,raw_data$lvngarrg)
clean_data$sp.livarrg<-ordered(clean_data$sp.livarrg,
                               levels=c(1,2,3,4),
                               labels=c("1: Alone", "2: With spouse/partner in household", "3: With spouse/partner and with others", "4: With others only"))
table(raw_data$lvngarrg, clean_data$sp.livarrg, exclude=NULL)
                              
#Number in HH -- includes SP (from R1-8 User guide -- note: appears 3 people have value 0 in R6. Recorded)
table(raw_data$hshldnum, exclude=NULL)
table(raw_data$hshldnum, clean_data$sp.livarrg, exclude=NULL)
clean_data$hh.number<-ifelse(raw_data$hshldnum==0,1,raw_data$hshldnum)
table(raw_data$hshldnum, clean_data$hh.number, exclude=NULL)
table(clean_data$hh.number, clean_data$sp.livarrg, exclude=NULL)

#Number children in HH
table(raw_data$hshldchd, exclude=NULL)
clean_data$hh.numchld<-raw_data$hshldchd
table(raw_data$hshldchd, clean_data$hh.numchld, exclude=NULL)

#Comorbidities -- currently coded as "lifetime hx of x"
table(raw_data$disescn3, exclude=NULL)
clean_data$sr.highbp<-ifelse(raw_data$disescn3==1,1, #yes=1
                           ifelse(raw_data$disescn3==2,0,#no=0
                                  ifelse(raw_data$disescn3==7,1, #prev reported=1
                                         ifelse(raw_data$disescn3==-8,NA,NA))))
table(raw_data$disescn3, clean_data$sr.highbp, exclude=NULL)
table(clean_data$sr.highbp, clean_data$round, exclude=NULL)

table(raw_data$disescn6, exclude=NULL)
clean_data$sr.diabetes<-ifelse(raw_data$disescn6==1,1,
                           ifelse(raw_data$disescn6==2,0,
                                  ifelse(raw_data$disescn6==7,1,
                                         ifelse(raw_data$disescn6==-8,NA,
                                          ifelse(raw_data$disescn6==-7,NA,NA)))))
table(raw_data$disescn6, clean_data$sr.diabetes, exclude=NULL)

table(raw_data$disescn8, exclude=NULL)
clean_data$sr.stroke<-ifelse(raw_data$disescn8==1,1,
                            ifelse(raw_data$disescn8==2,0,
                                   ifelse(raw_data$disescn8==7,1,
                                          ifelse(raw_data$disescn8==-8,NA,
                                                 ifelse(raw_data$disescn8==-7,NA,NA)))))
table(raw_data$disescn8, clean_data$stroke.bin, exclude=NULL)

table(raw_data$disescn9, exclude=NULL)
clean_data$sr.demalz<-ifelse(raw_data$disescn9==1,1,
                          ifelse(raw_data$disescn9==2,0,
                                 ifelse(raw_data$disescn9==7,1,
                                        ifelse(raw_data$disescn9==-8,NA,
                                               ifelse(raw_data$disescn9==-7,NA,NA)))))
table(raw_data$disescn9, clean_data$sr.demalz, exclude=NULL)

table(raw_data$disescn10, exclude=NULL)
clean_data$sr.cancer<-ifelse(raw_data$disescn10==1,1,
                           ifelse(raw_data$disescn10==2,0,
                                  ifelse(raw_data$disescn10==7,1,
                                         ifelse(raw_data$disescn10==-8,NA,
                                                ifelse(raw_data$disescn10==-7,NA,NA)))))
table(raw_data$disescn10, clean_data$sr.cancer, exclude=NULL)

#AD8 score
  table(raw_data$ad8_score, exclude=NULL)
  table(raw_data$ad8_flag, exclude=NULL) #Flag variable currently captures people with ad8.score=8 AND proxy reported Dem/Alz in AD8 in R1-R8
  table(raw_data$ad8_score, raw_data$ad8_proxydem, exclude=NULL) #If Proxy reported Dem/Alz in AD8 questions, then ad8_score = 8.
   
clean_data$ad8.score<- ifelse(raw_data$ad8_score==-1,NA,raw_data$ad8_score)
clean_data$ad8.demflag<- raw_data$ad8_flag

  table(raw_data$ad8_score, clean_data$ad8.score, exclude=NULL)
  table(clean_data$ad8.demflag, raw_data$round,exclude=NULL)
  table(clean_data$ad8.demflag, clean_data$ad8.score, exclude=NULL)
  
#Orientation Domain: sum of date recall and pres/vp name tests
table(raw_data$datena_score, exclude=NULL)
clean_data$datena.sum <- raw_data$datena_score
  table(raw_data$datena_score, clean_data$datena.sum, exclude=NULL)  
  
#Executive function domain: cock drawing score
table(raw_data$clock_scorer, exclude=NULL)
clean_data$clock.score<-raw_data$clock_scorer

  table(raw_data$clock_scorer, clean_data$clock.score, exclude=NULL)

#Memory domain: immediate and delayed word recall
  table(raw_data$wordrecall0_20, exclude=NULL)
  table(raw_data$wrdimmrc, raw_data$wrddlyrc, exclude=NULL)

clean_data$wrdrecall.sum<- ifelse(raw_data$wordrecall0_20==-18,NA,raw_data$wordrecall0_20)
  
  table(raw_data$wordrecall0_20, clean_data$wrdrecall.sum, exclude=NULL)  

#Complete cases  
clean_data$comp.case.HRQoL<-complete.cases(clean_data[c("spid", "round", "first.obs", "count.obs", "last.obs", "age.cat", "baseline.age", "dementia.status", 
                                                      "dementia.bin", "proxy", "analytic.wgt", "baseline.anwgt", "average.anwgt", "stratum", "baseline.stratum",
                                                      "cluster", "baseline.cluster", "race.eth", "prob.dep", "prob.anx", "poorhealth.bin", "pain.bother",
                                                      "funclimits", "female")])

clean_data$comp.case.WBQoL<-complete.cases(clean_data[c("spid", "round", "first.obs", "count.obs", "last.obs", "age.cat", "baseline.age", "dementia.status", 
                                                        "dementia.bin", "proxy", "analytic.wgt", "baseline.anwgt", "average.anwgt", "stratum", "baseline.stratum",
                                                        "cluster", "baseline.cluster", "race.eth", "wbscore", "female")])

clean_data$comp.case.all<-complete.cases(clean_data[c("spid", "round", "first.obs", "count.obs", "last.obs", "age.cat", "baseline.age", "dementia.status", 
                                                        "dementia.bin", "proxy", "analytic.wgt", "baseline.anwgt", "average.anwgt", "stratum", "baseline.stratum",
                                                        "prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits",
                                                        "cluster", "baseline.cluster", "race.eth", "wbscore", "female")])

table(clean_data$comp.case.HRQoL, exclude=NULL)
table(clean_data$comp.case.WBQoL, exclude=NULL)
table(clean_data$comp.case.all, exclude=NULL)

#------------------------------------------------------------------
# Save clean data and remove temporary objects
#------------------------------------------------------------------
save(clean_data, file="C:/Users/tmobley/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean.RData")

rm(list=ls(pattern="temp"))




