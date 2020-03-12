#------------------------------------------------------------------
# Title: HRQoL main analysis
# Author: Eleanor Hayes-Larson
#------------------------------------------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "survey", "tableone", "openxlsx")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean.RData")

#------------------------------------------------------------------
# Single wave results
#------------------------------------------------------------------

#Subset to complete cases with prob/poss dementia for all HRQoL vars
#Limit to black, white, hispanic due to sample size (can add "other" later when pooling)
clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

  #Create scaled weight:
  scalefactor<-nrow(clean_data_hrqol)/sum(clean_data_hrqol$w5anfinwgt0)
  clean_data_hrqol$scaled.wt<-clean_data_hrqol$w5anfinwgt0*scalefactor
  
clean_data_hrqol_dem<-clean_data_hrqol[clean_data_hrqol$dementia.bin==1,]
clean_data_hrqol_nodem<-clean_data_hrqol[clean_data_hrqol$dementia.bin==0,]
outcomes<-c("prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")


#Input complex survey design for analyses
  #Weights only
    nhats_design_dem_wtonly<-svydesign(data=clean_data_hrqol[clean_data_hrqol$dementia.bin==1,], 
                                       ids=~0, weights=~w5anfinwgt0, nest=T)
    nhats_design_nodem_wtonly<-svydesign(data=clean_data_hrqol[clean_data_hrqol$dementia.bin==0,], 
                                         ids=~0, weights=~w5anfinwgt0, nest=T)

  #With complex survey design
    nhats_design_dem<-svydesign(data=clean_data_hrqol[clean_data_hrqol$dementia.bin==1,], 
                                id=~w5varunit, strata=~w5varstrat, weights=~w5anfinwgt0, nest=T)
    nhats_design_nodem<-svydesign(data=clean_data_hrqol[clean_data_hrqol$dementia.bin==0,], 
                                  id=~w5varunit, strata=~w5varstrat, weights=~w5anfinwgt0, nest=T)



modelfxn<-function(indesign){
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_ser=rep(NA,5), hispanic_ser=rep(NA,5),  other_ser=rep(NA,5),
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    model<-svyglm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), 
                  design=indesign, family=poisson(link=log))
    summary(model)
    for (j in 2:4){
      outresults[i,j] <- exp(coef(model)[j])
      outresults[i,j+3] <- summary(model)$coefficients[j,"Std. Error"]
      outresults[i,j+6] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std. Error"])
      outresults[i,j+9] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std. Error"])
    }
  }
  return(outresults)
}


results_weighted_dem<-modelfxn(nhats_design_dem_wtonly)      
results_weighted_nodem<-modelfxn(nhats_design_nodem_wtonly)    
results_survey_dem<-modelfxn(nhats_design_dem)      
results_survey_nodem<-modelfxn(nhats_design_nodem)   


glmfxn<-function(indata, robust){
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_ser=rep(NA,5), hispanic_ser=rep(NA,5),  other_ser=rep(NA,5),
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-glm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), 
               data=indata, weights=scaled.wt, family=poisson(link=log))
    summary(model)
    
    #calculate robust SE
    cov.m1 <- vcovHC(model, type = "HC0")
    robust.std.err <- sqrt(diag(cov.m1))
    robust.std.err
    
    for (j in 2:4){
      outresults[i,j] <- exp(coef(model)[j])
      outresults[i,j+3] <- if (robust==1){robust.std.err[j]} else {summary(model)$coefficients[j,"Std. Error"]}
      outresults[i,j+6] <- exp(coef(model)[j]-1.96*outresults[i,j+3])
      outresults[i,j+9] <- exp(coef(model)[j]+1.96*outresults[i,j+3])
    }
  }
  return(outresults)
}

results_modelse_dem<-glmfxn(clean_data_hrqol_dem, 0)      
results_modelse_nodem<-glmfxn(clean_data_hrqol_nodem, 0)    
results_robustse_dem<-glmfxn(clean_data_hrqol_dem, 1)      
results_robustse_nodem<-glmfxn(clean_data_hrqol_nodem, 1)   



geefxn<-function(indata){
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_ser=rep(NA,5), hispanic_ser=rep(NA,5),  other_ser=rep(NA,5),
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    model<-geeglm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), 
                  id=spid, data=indata, corstr="exchangeable", weights=scaled.wt, family=poisson(link=log))
    summary(model)
    
    for (j in 2:4){
      outresults[i,j] <- exp(coef(model)[j])
      outresults[i,j+3] <- summary(model)$coefficients[j,"Std.err"]
      outresults[i,j+6] <- exp(coef(model)[j]-1.96*outresults[i,j+3])
      outresults[i,j+9] <- exp(coef(model)[j]+1.96*outresults[i,j+3])
    }
  }
  return(outresults)
}

results_gee_dem<-geefxn(clean_data_hrqol_dem)      
results_gee_nodem<-geefxn(clean_data_hrqol_nodem)    


#Comparing SEs.
black_SEratio_survwt<-results_survey_dem$black_ser/results_weighted_dem$black_ser
black_SEratio_survglmmodel<-results_survey_dem$black_ser/results_modelse_dem$black_ser
black_SEratio_survglmrobust<-results_survey_dem$black_ser/results_robustse_dem$black_ser
black_SEratio_survgee<-results_survey_dem$black_ser/results_gee_dem$black_ser

blackSEratios<-cbind(black_SEratio_survwt,black_SEratio_survglmmodel, 
                     black_SEratio_survglmrobust, black_SEratio_survgee)


hispanic_SEratio_survwt<-results_survey_dem$hispanic_ser/results_weighted_dem$hispanic_ser
hispanic_SEratio_survglmmodel<-results_survey_dem$hispanic_ser/results_modelse_dem$hispanic_ser
hispanic_SEratio_survglmrobust<-results_survey_dem$hispanic_ser/results_robustse_dem$hispanic_ser
hispanic_SEratio_survgee<-results_survey_dem$hispanic_ser/results_gee_dem$hispanic_ser

hispanicSEratios<-cbind(hispanic_SEratio_survwt,hispanic_SEratio_survglmmodel, 
                        hispanic_SEratio_survglmrobust, hispanic_SEratio_survgee)


other_SEratio_survwt<-results_survey_dem$other_ser/results_weighted_dem$other_ser
other_SEratio_survglmmodel<-results_survey_dem$other_ser/results_modelse_dem$other_ser
other_SEratio_survglmrobust<-results_survey_dem$other_ser/results_robustse_dem$other_ser
other_SEratio_survgee<-results_survey_dem$other_ser/results_gee_dem$other_ser

otherSEratios<-cbind(other_SEratio_survwt,other_SEratio_survglmmodel, 
                     other_SEratio_survglmrobust, other_SEratio_survgee)


SE.ratios_dem<-cbind(blackSEratios, hispanicSEratios, otherSEratios)
SE.ratios_dem

#Comparing ests.
black_estratio_survwt<-results_survey_dem$black_est/results_weighted_dem$black_est
black_estratio_survglmmodel<-results_survey_dem$black_est/results_modelse_dem$black_est
black_estratio_survglmrobust<-results_survey_dem$black_est/results_robustse_dem$black_est
black_estratio_survgee<-results_survey_dem$black_est/results_gee_dem$black_est

blackestratios<-cbind(black_estratio_survwt,black_estratio_survglmmodel, 
                      black_estratio_survglmrobust, black_estratio_survgee)


hispanic_estratio_survwt<-results_survey_dem$hispanic_est/results_weighted_dem$hispanic_est
hispanic_estratio_survglmmodel<-results_survey_dem$hispanic_est/results_modelse_dem$hispanic_est
hispanic_estratio_survglmrobust<-results_survey_dem$hispanic_est/results_robustse_dem$hispanic_est
hispanic_estratio_survgee<-results_survey_dem$hispanic_est/results_gee_dem$hispanic_est

hispanicestratios<-cbind(hispanic_estratio_survwt,hispanic_estratio_survglmmodel, 
                         hispanic_estratio_survglmrobust,hispanic_estratio_survgee )


other_estratio_survwt<-results_survey_dem$other_est/results_weighted_dem$other_est
other_estratio_survglmmodel<-results_survey_dem$other_est/results_modelse_dem$other_est
other_estratio_survglmrobust<-results_survey_dem$other_est/results_robustse_dem$other_est
other_estratio_survgee<-results_survey_dem$other_est/results_gee_dem$other_est

otherestratios<-cbind(other_estratio_survwt,other_estratio_survglmmodel, 
                      other_estratio_survglmrobust,other_estratio_survgee)


est.ratios_dem<-cbind(blackestratios, hispanicestratios, otherestratios)
est.ratios_dem


#NO DEMENTIA GROUP


#Comparing SEs.
black_SEratio_survwt<-results_survey_nodem$black_ser/results_weighted_nodem$black_ser
black_SEratio_survglmmodel<-results_survey_nodem$black_ser/results_modelse_nodem$black_ser
black_SEratio_survglmrobust<-results_survey_nodem$black_ser/results_robustse_nodem$black_ser
black_SEratio_survgee<-results_survey_nodem$black_ser/results_gee_nodem$black_ser

blackSEratios<-cbind(black_SEratio_survwt,black_SEratio_survglmmodel, 
                     black_SEratio_survglmrobust, black_SEratio_survgee)


hispanic_SEratio_survwt<-results_survey_nodem$hispanic_ser/results_weighted_nodem$hispanic_ser
hispanic_SEratio_survglmmodel<-results_survey_nodem$hispanic_ser/results_modelse_nodem$hispanic_ser
hispanic_SEratio_survglmrobust<-results_survey_nodem$hispanic_ser/results_robustse_nodem$hispanic_ser
hispanic_SEratio_survgee<-results_survey_nodem$hispanic_ser/results_gee_nodem$hispanic_ser

hispanicSEratios<-cbind(hispanic_SEratio_survwt,hispanic_SEratio_survglmmodel, 
                        hispanic_SEratio_survglmrobust, hispanic_SEratio_survgee)


other_SEratio_survwt<-results_survey_nodem$other_ser/results_weighted_nodem$other_ser
other_SEratio_survglmmodel<-results_survey_nodem$other_ser/results_modelse_nodem$other_ser
other_SEratio_survglmrobust<-results_survey_nodem$other_ser/results_robustse_nodem$other_ser
other_SEratio_survgee<-results_survey_nodem$other_ser/results_gee_nodem$other_ser

otherSEratios<-cbind(other_SEratio_survwt,other_SEratio_survglmmodel, 
                     other_SEratio_survglmrobust, other_SEratio_survgee)


SE.ratios_nodem<-cbind(blackSEratios, hispanicSEratios, otherSEratios)
SE.ratios_nodem

#Comparing ests.
black_estratio_survwt<-results_survey_nodem$black_est/results_weighted_nodem$black_est
black_estratio_survglmmodel<-results_survey_nodem$black_est/results_modelse_nodem$black_est
black_estratio_survglmrobust<-results_survey_nodem$black_est/results_robustse_nodem$black_est
black_estratio_survgee<-results_survey_nodem$black_est/results_gee_nodem$black_est

blackestratios<-cbind(black_estratio_survwt,black_estratio_survglmmodel, 
                      black_estratio_survglmrobust, black_estratio_survgee)


hispanic_estratio_survwt<-results_survey_nodem$hispanic_est/results_weighted_nodem$hispanic_est
hispanic_estratio_survglmmodel<-results_survey_nodem$hispanic_est/results_modelse_nodem$hispanic_est
hispanic_estratio_survglmrobust<-results_survey_nodem$hispanic_est/results_robustse_nodem$hispanic_est
hispanic_estratio_survgee<-results_survey_nodem$hispanic_est/results_gee_nodem$hispanic_est

hispanicestratios<-cbind(hispanic_estratio_survwt,hispanic_estratio_survglmmodel, 
                         hispanic_estratio_survglmrobust,hispanic_estratio_survgee )


other_estratio_survwt<-results_survey_nodem$other_est/results_weighted_nodem$other_est
other_estratio_survglmmodel<-results_survey_nodem$other_est/results_modelse_nodem$other_est
other_estratio_survglmrobust<-results_survey_nodem$other_est/results_robustse_nodem$other_est
other_estratio_survgee<-results_survey_nodem$other_est/results_gee_nodem$other_est

otherestratios<-cbind(other_estratio_survwt,other_estratio_survglmmodel, 
                      other_estratio_survglmrobust,other_estratio_survgee)


est.ratios_nodem<-cbind(blackestratios, hispanicestratios, otherestratios)
est.ratios_nodem


#------------------------------------------------------------------
# Save tables #
#------------------------------------------------------------------
SE.list <- list("Dementia" = SE.ratios_dem, "No dementia" = SE.ratios_nodem)
write.xlsx(SE.list, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/SE exploration.xlsx")






#------------------------------------------------------------------
# Pooled data
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")
clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

#Create scaled weight:
scalefactor<-nrow(clean_data_hrqol)/sum(clean_data_hrqol$baseline.anwgt)
clean_data_hrqol$scaled.wt<-clean_data_hrqol$baseline.anwgt*scalefactor


clean_data_hrqol_dem<-clean_data_hrqol[clean_data_hrqol$dementia.bin==1,]
clean_data_hrqol_nodem<-clean_data_hrqol[clean_data_hrqol$dementia.bin==0,]





model<-geeglm(as.formula(paste("prob.dep","~as.factor(race.eth)+as.factor(age.cat)+female")), 
              id=spid, data=clean_data_hrqol_dem, corstr="exchangeable", weights=scaled.wt, family=poisson(link=log))
summary(model)


nhats_design_dem2<-svydesign(data=clean_data_hrqol[clean_data_hrqol$dementia.bin==1,], 
                             id=~cluster, strata=~stratum, weights=~baseline.anwgt, nest=T)

model2<-svyglm(as.formula(paste("prob.dep","~as.factor(race.eth)+as.factor(age.cat)+female")), 
               design=nhats_design_dem2, family=poisson(link=log))
summary(model2)


nhats_design_dem3<-svydesign(data=clean_data_hrqol[clean_data_hrqol$dementia.bin==1,], 
                             ids=~cluster+spid, strata=~stratum, weights=~baseline.anwgt, nest=T)
model3<-svyglm(as.formula(paste("prob.dep","~as.factor(race.eth)+as.factor(age.cat)+female")), 
               design=nhats_design_dem3, family=poisson(link=log))
summary(model3)


model4<-glm(as.formula(paste("prob.dep","~as.factor(race.eth)+as.factor(age.cat)+female")), 
            data=clean_data_hrqol_dem, weights=scaled.wt, family=poisson(link=log))

cov.m1 <- vcovHC(model4, type = "HC0")
robust.std.err <- sqrt(diag(cov.m1))

r.est <- cbind(Estimate= coef(model4), "Robust SE" = robust.std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(model4)/robust.std.err), lower.tail=FALSE),
               LL = coef(model4) - 1.96 * robust.std.err,
               UL = coef(model4) + 1.96 * robust.std.err)

r.est



#Unweighted
model<-geeglm(as.formula(paste("prob.dep","~as.factor(race.eth)+as.factor(age.cat)+female")), 
              id=spid, data=clean_data_hrqol_dem, corstr="independence", family=poisson(link=log))
summary(model)

nullweight<-rep(1,nrow(clean_data_hrqol_dem))
nhats_design_dem2<-svydesign(data=clean_data_hrqol_dem, 
                             id=~spid, strata=NULL, weights=~nullweight, nest=F)
model2<-svyglm(as.formula(paste("prob.dep","~as.factor(race.eth)+as.factor(age.cat)+female")), 
               design=nhats_design_dem2, family=poisson(link=log))
summary(model2)



model4<-glm(as.formula(paste("prob.dep","~as.factor(race.eth)+as.factor(age.cat)+female")), 
            data=clean_data_hrqol_dem, family=poisson(link=log))

cov.m1 <- vcovHC(model4, type = "HC0")
robust.std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(model4), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(model4)/std.err), lower.tail=FALSE),
               LL = coef(model4) - 1.96 * std.err,
               UL = coef(model4) + 1.96 * std.err)

r.est



install.packages("ClusterBootstrap")
library(ClusterBootstrap)


test<-clusbootglm(
  as.formula(paste("prob.dep","~as.factor(race.eth)+as.factor(age.cat)+female")),
  data=clean_data_hrqol_dem,
  clusterid=spid,
  family = poisson (link=log),
  B = 5,
  confint.level = 0.95,
  n.cores = 1
)

summary(test)
