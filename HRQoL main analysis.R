#------------------------------------------------------------------
# Title: HRQoL main analysis
# Author: Eleanor Hayes-Larson
#------------------------------------------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "survey", "tableone")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean.RData")

#Subset to complete cases with prob/poss dementia for all HRQoL vars
#Limit to black, white, hispanic due to sample size (can add "other" later when pooling)
clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1 & clean_data$dementia.bin==1,]

#Input complex survey design for analysis
nhats_design<-svydesign(data=clean_data_hrqol, id=~w5varunit, strata=~w5varstrat, weights=~w5anfinwgt0, nest=T)


#Sample descriptives

    #unweighted
      catvars<-c("age.cat", "female", "race.eth", "prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")
      CreateTableOne(vars=catvars, data=clean_data_hrqol, factorVars=catvars)

    #weighted
      svymean(~female, nhats_design)
      svymean(~as.factor(race.eth), nhats_design)
      svymean(~as.factor(age.cat), nhats_design)
      svymean(~prob.dep, nhats_design)
      svymean(~prob.anx, nhats_design)
      svymean(~poorhealth.bin, nhats_design)
      svymean(~pain.bother, nhats_design)
      svymean(~funclimits, nhats_design)


#Relative risk regression models
      
outcomes<-c("prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")
results_weighted<-data.frame(outcome=outcomes,black_RR=rep(NA,5), black_lci=rep(NA,5), black_uci=rep(NA,5), hispanic_RR=rep(NA,5), hispanic_lci=rep(NA,5), hispanic_uci=rep(NA,5), other_RR=rep(NA,5), other_lci=rep(NA,5), other_uci=rep(NA,5))
results_unweighted<-data.frame(outcome=outcomes,black_RR=rep(NA,5), black_lci=rep(NA,5), black_uci=rep(NA,5), hispanic_RR=rep(NA,5), hispanic_lci=rep(NA,5), hispanic_uci=rep(NA,5), other_RR=rep(NA,5), other_lci=rep(NA,5), other_uci=rep(NA,5))

#weighted
    for (i in 1:length(outcomes)){
    outcome=colnames(clean_data_hrqol)[i+7]
    model<-svyglm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), design=nhats_design, family=quasibinomial(link=log), start=c(-0.5,rep(0,9)))
    results_weighted$black_RR[i]<-exp(coef(model)[2])
    results_weighted$hispanic_RR[i]<-exp(coef(model)[3])
    results_weighted$other_RR[i]<-exp(coef(model)[4])
    results_weighted$black_lci[i]<-exp(confint(model)[2,1])
    results_weighted$black_uci[i]<-exp(confint(model)[2,2])
    results_weighted$hispanic_lci[i]<-exp(confint(model)[3,1])
    results_weighted$hispanic_uci[i]<-exp(confint(model)[3,2])
    results_weighted$other_lci[i]<-exp(confint(model)[4,1])
    results_weighted$other_uci[i]<-exp(confint(model)[4,2])
    }

#unweighted
for (i in 1:length(outcomes)){
  outcome=colnames(clean_data_hrqol)[i+7]
  model<-glm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), data=clean_data_hrqol, family=binomial(link=log))
  results_unweighted$black_RR[i]<-exp(coef(model)[2])
  results_unweighted$hispanic_RR[i]<-exp(coef(model)[3])
  results_unweighted$other_RR[i]<-exp(coef(model)[4])
  results_unweighted$black_lci[i]<-exp(confint(model)[2,1])
  results_unweighted$black_uci[i]<-exp(confint(model)[2,2])
  results_unweighted$hispanic_lci[i]<-exp(confint(model)[3,1])
  results_unweighted$hispanic_uci[i]<-exp(confint(model)[3,2])
  results_unweighted$other_lci[i]<-exp(confint(model)[4,1])
  results_unweighted$other_uci[i]<-exp(confint(model)[4,2])
}

