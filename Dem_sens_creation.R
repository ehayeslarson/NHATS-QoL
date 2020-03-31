#------------------------------------------------------------------
# Title: Sensitivity analysis exploration
# Author: Eleanor Hayes-Larson
#------------------------------------------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "gee", "geepack",  
       "survey", "tableone", "openxlsx", "emmeans", "rlang", "rccmisc")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")



#Keep only Round 1, self-respondents to be consistent with NHATS algorithm

threshold_samp<-clean_data[clean_data$round==1 & clean_data$proxy==0,]
nhats_design<-svydesign(data=threshold_samp, id=~cluster, strata=~stratum, weights=~analytic.wgt, nest=T)

clock.mean<-svymean(~clock.score, nhats_design)[1]
clock.sd<-sqrt(svyvar(~clock.score, nhats_design))[1]

word.mean<-svymean(~wrdrecall.sum, nhats_design)[1]
word.sd<-sqrt(svyvar(~wrdrecall.sum, nhats_design))[1]

datepres.mean<-svymean(~datena.sum, nhats_design)[1]
datepres.sd<-sqrt(svyvar(~datena.sum, nhats_design))[1]

overall<-data.frame(threshold=c("1.25SD", "1.50SD", "1.75SD"), clock=rep(NA,3), word=rep(NA,3), datepres=rep(NA,3))

SDmults<-c(1.25, 1.50, 1.75)

overall$clock<-floor(clock.mean-SDmults*clock.sd)
overall$word<-floor(word.mean-SDmults*word.sd)
overall$datepres<-floor(datepres.mean-SDmults*datepres.sd)

overall

clock.stratmeans<-svyby(~clock.score, ~race.eth, nhats_design, svymean)[,1:2]
clock.stratsds<-sqrt(svyby(~clock.score, ~race.eth, nhats_design, svyvar)[,2])
clock.thresholds<-floor(clock.stratmeans[2]-1.5*clock.stratsds )

word.stratmeans<-svyby(~wrdrecall.sum, ~race.eth, nhats_design, svymean)[,1:2]
word.stratsds<-sqrt(svyby(~wrdrecall.sum, ~race.eth, nhats_design, svyvar)[,2])
word.thresholds<-floor(word.stratmeans[2]-1.5*word.stratsds )

datepres.stratmeans<-svyby(~datena.sum, ~race.eth, nhats_design, svymean)[,1:2]
datepres.stratsds<-sqrt(svyby(~datena.sum, ~race.eth, nhats_design, svyvar)[,2])
datepres.thresholds<-floor(datepres.stratmeans[2]-1.5*datepres.stratsds )

base<-cbind(clock.stratmeans[1],clock.thresh=1, word.thresholds=3, datepres.thresholds=3)
names(base)<-c("race.eth", "clock.thresh0", "word.thresh0", "datepres.thresh0")


sens1<-cbind(clock.stratmeans[1],clock.thresh=clock.thresholds, word.thresholds, datepres.thresholds)
names(sens1)<-c("race.eth", "clock.thresh1", "word.thresh1", "datepres.thresh1")

sens2<-data.frame(race.eth=c(1:4), 
                  clock.thresh2=c(overall$clock[overall$threshold=="1.50SD"], 
                                 rep(overall$clock[overall$threshold=="1.75SD"],3)),
                  word.thresh2=c(overall$word[overall$threshold=="1.50SD"], 
                                 rep(overall$word[overall$threshold=="1.75SD"],3)),
                  datepres.thresh2=c(overall$datepres[overall$threshold=="1.50SD"], 
                     rep(overall$datepres[overall$threshold=="1.75SD"],3)))


sens3<-data.frame(race.eth=c(1:4), 
                  clock.thresh3=c(overall$clock[overall$threshold=="1.25SD"], 
                                 rep(overall$clock[overall$threshold=="1.50SD"],3)),
                  word.thresh3=c(overall$word[overall$threshold=="1.25SD"], 
                                rep(overall$word[overall$threshold=="1.50SD"],3)),
                  datepres.thresh3=c(overall$datepres[overall$threshold=="1.25SD"], 
                                    rep(overall$datepres[overall$threshold=="1.50SD"],3)))


sens4<-data.frame(race.eth=c(1:4), 
                  clock.thresh4=c(overall$clock[overall$threshold=="1.25SD"], 
                                 rep(overall$clock[overall$threshold=="1.75SD"],3)),
                  word.thresh4=c(overall$word[overall$threshold=="1.25SD"], 
                                rep(overall$word[overall$threshold=="1.75SD"],3)),
                  datepres.thresh4=c(overall$datepres[overall$threshold=="1.25SD"], 
                                    rep(overall$datepres[overall$threshold=="1.75SD"],3)))


clean_data<-merge(clean_data, base, all.x=T, by="race.eth", suffixes=c("", "0"))
clean_data<-merge(clean_data, sens1, all.x=T, by="race.eth", suffixes=c("", "1"))
clean_data<-merge(clean_data, sens2, all.x=T, by="race.eth", suffixes=c("", "2"))
clean_data<-merge(clean_data, sens3, all.x=T, by="race.eth", suffixes=c("", "3"))
clean_data<-merge(clean_data, sens4, all.x=T, by="race.eth", suffixes=c("", "4"))



for (i in 0:4) {
    clean_data[, paste0("meets.clock",i)] <- ifelse(clean_data$clock.score<=clean_data[,paste0("clock.thresh",i)],1,0)
    clean_data[, paste0("meets.word",i)] <- ifelse(clean_data$wrdrecall.sum<=clean_data[,paste0("word.thresh",i)],1,0)
    clean_data[, paste0("meets.datepres",i)] <- ifelse(clean_data$datena.sum<=clean_data[,paste0("datepres.thresh",i)],1,0)
    
    clean_data[,paste0("domain.sum",i)]<-rowSums(clean_data[, c(paste0("meets.clock",i), paste0("meets.word",i),paste0("meets.datepres",i))], na.rm=T)
    
    }


clean_data[,c("dem_sens0","dem_sens1","dem_sens2","dem_sens3","dem_sens4","probdem_sens0","probdem_sens1","probdem_sens2","probdem_sens3","probdem_sens4")]<-NA
clean_data<-clean_data %>% mutate(dem_sens0=replace(dem_sens0, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum0>=1), 1))
clean_data<-clean_data %>% mutate(dem_sens0=replace(dem_sens0, (is.na(clean_data$dem_sens0)), 0))

clean_data<-clean_data %>% mutate(probdem_sens0=replace(probdem_sens0, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum0>=2), 1))
clean_data<-clean_data %>% mutate(probdem_sens0=replace(probdem_sens0, (is.na(clean_data$probdem_sens0)), 0))

clean_data<-clean_data %>% mutate(dem_sens1=replace(dem_sens1, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum1>=1), 1))
clean_data<-clean_data %>% mutate(dem_sens1=replace(dem_sens1, (is.na(clean_data$dem_sens1)), 0))

clean_data<-clean_data %>% mutate(probdem_sens1=replace(probdem_sens1, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum1>=2), 1))
clean_data<-clean_data %>% mutate(probdem_sens1=replace(probdem_sens1, (is.na(clean_data$probdem_sens1)), 0))

clean_data<-clean_data %>% mutate(dem_sens2=replace(dem_sens2, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum2>=1), 1))
clean_data<-clean_data %>% mutate(dem_sens2=replace(dem_sens2, (is.na(clean_data$dem_sens2)), 0))

clean_data<-clean_data %>% mutate(probdem_sens2=replace(probdem_sens2, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum2>=2), 1))
clean_data<-clean_data %>% mutate(probdem_sens2=replace(probdem_sens2, (is.na(clean_data$probdem_sens2)), 0))

clean_data<-clean_data %>% mutate(dem_sens3=replace(dem_sens3, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum3>=1), 1))
clean_data<-clean_data %>% mutate(dem_sens3=replace(dem_sens3, (is.na(clean_data$dem_sens3)), 0))

clean_data<-clean_data %>% mutate(probdem_sens3=replace(probdem_sens3, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum3>=2), 1))
clean_data<-clean_data %>% mutate(probdem_sens3=replace(probdem_sens3, (is.na(clean_data$probdem_sens3)), 0))

clean_data<-clean_data %>% mutate(dem_sens4=replace(dem_sens4, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum4>=1), 1))
clean_data<-clean_data %>% mutate(dem_sens4=replace(dem_sens4, (is.na(clean_data$dem_sens4)), 0))

clean_data<-clean_data %>% mutate(probdem_sens4=replace(probdem_sens4, (clean_data$sr.demalz==1 | clean_data$ad8.score>=2 | clean_data$domain.sum4>=2), 1))
clean_data<-clean_data %>% mutate(probdem_sens4=replace(probdem_sens4, (is.na(clean_data$probdem_sens4)), 0))


clean_data$probdementia.bin<-ifelse(clean_data$dementia.status==1, 1, 0)

clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]



#Check impact of sensitivity analyses
table(clean_data_hrqol$dementia.bin, clean_data_hrqol$dem_sens0, exclude=NULL) #0. Check for match with original dementia var
table(clean_data_hrqol$dementia.bin, clean_data_hrqol$dem_sens1, clean_data_hrqol$race.eth,exclude=NULL) #1. Use race-specific 1.5SD 
table(clean_data_hrqol$dementia.bin, clean_data_hrqol$dem_sens2, clean_data_hrqol$race.eth, exclude=NULL) #2. Make threshold stricter for non-whites
table(clean_data_hrqol$dementia.bin, clean_data_hrqol$dem_sens3, clean_data_hrqol$race.eth, exclude=NULL) #3. Make threshold less strict for whites
table(clean_data_hrqol$dementia.bin, clean_data_hrqol$dem_sens4, clean_data_hrqol$race.eth, exclude=NULL) #4. Both 2 and 3

table(clean_data_hrqol$age.cat,  clean_data_hrqol$dementia.bin, clean_data_hrqol$race.eth,exclude=NULL) #1. Use original var
table(clean_data_hrqol$age.cat,  clean_data_hrqol$dem_sens1, clean_data_hrqol$race.eth,exclude=NULL) #1. Use race-specific 1.5SD 
table(clean_data_hrqol$age.cat,  clean_data_hrqol$dem_sens2, clean_data_hrqol$race.eth,exclude=NULL) #2. Make threshold stricter for non-whites
table(clean_data_hrqol$age.cat,  clean_data_hrqol$dem_sens3, clean_data_hrqol$race.eth,exclude=NULL) #3. Make threshold less strict for whites
table(clean_data_hrqol$age.cat,  clean_data_hrqol$dem_sens4, clean_data_hrqol$race.eth,exclude=NULL) #4. Both 2 and 3


#Check impact of sensitivity analyses on probable-only dementia
table(clean_data_hrqol$probdementia.bin, clean_data_hrqol$probdem_sens0, exclude=NULL) #0. Check for match with original probdementia var
table(clean_data_hrqol$probdementia.bin, clean_data_hrqol$probdem_sens1, clean_data_hrqol$race.eth,exclude=NULL) #1. Use race-specific 1.5SD 
table(clean_data_hrqol$probdementia.bin, clean_data_hrqol$probdem_sens2, clean_data_hrqol$race.eth, exclude=NULL) #2. Make threshold stricter for non-whites
table(clean_data_hrqol$probdementia.bin, clean_data_hrqol$probdem_sens3, clean_data_hrqol$race.eth, exclude=NULL) #3. Make threshold less strict for whites
table(clean_data_hrqol$probdementia.bin, clean_data_hrqol$probdem_sens4, clean_data_hrqol$race.eth, exclude=NULL) #4. Both 2 and 3


table(clean_data_hrqol$age.cat,  clean_data_hrqol$probdementia.bin, clean_data_hrqol$race.eth,exclude=NULL) #1. Use original var 
table(clean_data_hrqol$age.cat,  clean_data_hrqol$probdem_sens1, clean_data_hrqol$race.eth,exclude=NULL) #1. Use race-specific 1.5SD 
table(clean_data_hrqol$age.cat,  clean_data_hrqol$probdem_sens2, clean_data_hrqol$race.eth,exclude=NULL) #2. Make threshold stricter for non-whites
table(clean_data_hrqol$age.cat,  clean_data_hrqol$probdem_sens3, clean_data_hrqol$race.eth,exclude=NULL) #3. Make threshold less strict for whites
table(clean_data_hrqol$age.cat,  clean_data_hrqol$probdem_sens4, clean_data_hrqol$race.eth,exclude=NULL) #4. Both 2 and 3
