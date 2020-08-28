#New code for R&R:
# Calculate proxy types
# Calculate financial strain variable


#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "gee", "wgeesel", 
       "survey", "tableone", "openxlsx", "rlang", "questionr")


#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")

clean_data<-clean_data[order(clean_data$spid, clean_data$round),]

clean_data<-clean_data %>%
  group_by(spid) %>%
  mutate(enrollment = round[which(first.obs==1)])

clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

#Keep baseline observation for participants in clean_data_hrqol
idlist<-clean_data_hrqol %>% distinct(spid)
idlist<-unlist(idlist[,1])
clean_data_hrqol_baseline<-clean_data %>% filter(.,first.obs==1, spid %in% idlist)

#summarize proxy types 
table(clean_data_hrqol_baseline$proxy.relat)

#Clean proxy types:
#2 = spouse
#3-daughter
# 4=son
# 5=daughter in law
# 6=son in law
# 7=stepdaughter
# 8=stepson
# 9=sister
# 10=brother
# 11=sisterinlaw
# 12=brotherinlaw
# 13=mother
# 14=stepmother
# 15=MIL
# 16=father
# 17=stepfather
# 18=Fil
# 19 = grandaughter
# 20 = grandson
# 21=niece
# 22=nephew
# 23=aunt
# 24=uncle
# 25=cousin
# 26=stepdaughters son/daughter
# 27=stepsons son/daughter
# 28 = DILs son/daughter
# 29 = SILs son/daughter
# 30 = boarder/renter
# 31=paid ade/housekeeper/employee
# 32=roommate
# 33=exwife/husband
# 34=BF/GF
# 35 neighter
# 36=friend
# 37=service/someone from place SP lives
# 91 = other relative
# 92 = other non-relative


proxysubset<-clean_data_hrqol_baseline %>% filter(proxy==1)

proxysubset$proxy_type<-ifelse(proxysubset$proxy.relat==2,"Spouse",
                                ifelse(proxysubset$proxy.relat %in% c(3:8), "Child",
                                  ifelse(proxysubset$proxy.relat %in% c(9:29,91), "Other relative", "Other non-relative")))

#Unweighted
CreateCatTable("proxy_type", "race.eth", data=proxysubset, test=F)

#Weighted
nhats_design_proxy<-svydesign(data=proxysubset, id=~cluster, weights=~analytic.wgt, nest=T)
svyCreateCatTable("proxy_type", "race.eth", data=nhats_design_proxy, includeNA=T, test=F)



#Financial hardship variables,  only available waves 2-8, 
#need to use wave 2 for people enrolled in wave 1, wave 5 for people enrolled in wave 5

wave1<- clean_data %>% filter(enrollment==1, round==2) %>% select(c(spid,
                                                                    econwb.house_clean=econwb.house,
                                                                    econwb.meal_clean=econwb.meal,
                                                                    econwb.util_clean=econwb.util,
                                                                    econwb.med_clean=econwb.med))
head(wave1)
wave5<-clean_data %>% filter(enrollment==5, round==5) %>% select(c(spid,
                                                                   econwb.house_clean=econwb.house,
                                                                   econwb.meal_clean=econwb.meal,
                                                                   econwb.util_clean=econwb.util,
                                                                   econwb.med_clean=econwb.med ))
head(wave5)

#Merge back selected econ wb waves
clean_data_hrqol_baseline<-left_join(clean_data_hrqol_baseline, rbind(wave1,wave5), by="spid")

#Any financial hardship defined as at least one of the 4:
clean_data_hrqol_baseline$anyecon_hardship<-pmax(
                                clean_data_hrqol_baseline$econwb.house_clean, 
                                clean_data_hrqol_baseline$econwb.meal_clean, 
                                clean_data_hrqol_baseline$econwb.util_clean, 
                                clean_data_hrqol_baseline$econwb.med_clean)


#Create tables

#Unweighted
CreateCatTable("anyecon_hardship", "race.eth", data=clean_data_hrqol_baseline, includeNA=F, test=F)

#WEighted 
nhats_design<-svydesign(data=clean_data_hrqol_baseline, id=~cluster, strata=~stratum, weights=~analytic.wgt, nest=T)
svyCreateCatTable("anyecon_hardship", "race.eth", data=nhats_design, includeNA=F, test=F)



#checking missingness
r1only<-clean_data_hrqol %>% filter(enrollment==1 & first.obs==1 & last.obs==1) %>% select(spid)


clean_data_hrqol_baseline$r1only<-ifelse(clean_data_hrqol_baseline$enrollment==1 & clean_data_hrqol_baseline$first.obs==1 & clean_data_hrqol_baseline$last.obs==1, 1, 0)
table(clean_data_hrqol_baseline$r1only)

#COmpare those only in W1 to rest of cohort at baseline.
catvars<-c("race.eth", "age.cat", "female","edu.7cat", "resid.care", "cens.area", "born.us", 
           "proxy", "proxy.fam", "sr.highbp","sr.diabetes","sr.stroke", "sr.cancer", "dementia.status", "round")
CreateCatTable(catvars, "r1only", clean_data_hrqol_baseline, includeNA=T, test=F)
