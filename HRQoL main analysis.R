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
# Analysis among dementa = 1 #
#------------------------------------------------------------------

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

      
#Check differences in proxy by race
      CreateTableOne(vars="proxy", strata="race.eth", data=clean_data_hrqol, factorVars=c("race.eth","proxy"))
     

#Relative risk regression models
      
outcomes<-c("prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")
results_weighted<-data.frame(outcome=outcomes,black_RR=rep(NA,5), black_lci=rep(NA,5), black_uci=rep(NA,5), hispanic_RR=rep(NA,5), hispanic_lci=rep(NA,5), hispanic_uci=rep(NA,5), other_RR=rep(NA,5), other_lci=rep(NA,5), other_uci=rep(NA,5))
results_unweighted<-data.frame(outcome=outcomes,black_RR=rep(NA,5), black_lci=rep(NA,5), black_uci=rep(NA,5), hispanic_RR=rep(NA,5), hispanic_lci=rep(NA,5), hispanic_uci=rep(NA,5), other_RR=rep(NA,5), other_lci=rep(NA,5), other_uci=rep(NA,5))

#weighted
    for (i in 1:length(outcomes)){
    outcome=colnames(clean_data_hrqol)[i+8]
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
  outcome=colnames(clean_data_hrqol)[i+8]
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

#------------------------------------------------------------------
# Analysis among dementa = 0 #
#------------------------------------------------------------------
#Subset to complete cases with prob/poss dementia for all HRQoL vars
#Limit to black, white, hispanic due to sample size (can add "other" later when pooling)
clean_data_hrqol_nodem<-clean_data[clean_data$comp.case.HRQoL==1 & clean_data$dementia.bin==0,]

#Input complex survey design for analysis
nhats_design_nodem<-svydesign(data=clean_data_hrqol_nodem, id=~w5varunit, strata=~w5varstrat, weights=~w5anfinwgt0, nest=T)


#Sample descriptives

#unweighted
CreateTableOne(vars=catvars, data=clean_data_hrqol_nodem, factorVars=catvars)

#weighted
svymean(~female, nhats_design_nodem)
svymean(~as.factor(race.eth), nhats_design_nodem)
svymean(~as.factor(age.cat), nhats_design_nodem)
svymean(~prob.dep, nhats_design_nodem)
svymean(~prob.anx, nhats_design_nodem)
svymean(~poorhealth.bin, nhats_design_nodem)
svymean(~pain.bother, nhats_design_nodem)
svymean(~funclimits, nhats_design_nodem)


#Check differences in proxy by race
CreateTableOne(vars="proxy", strata="race.eth", data=clean_data_hrqol, factorVars=c("race.eth","proxy"))


#Relative risk regression models

outcomes<-c("prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")
results_weighted_nodem<-data.frame(outcome=outcomes,black_RR=rep(NA,5), black_lci=rep(NA,5), black_uci=rep(NA,5), hispanic_RR=rep(NA,5), hispanic_lci=rep(NA,5), hispanic_uci=rep(NA,5), other_RR=rep(NA,5), other_lci=rep(NA,5), other_uci=rep(NA,5))
results_unweighted_nodem<-data.frame(outcome=outcomes,black_RR=rep(NA,5), black_lci=rep(NA,5), black_uci=rep(NA,5), hispanic_RR=rep(NA,5), hispanic_lci=rep(NA,5), hispanic_uci=rep(NA,5), other_RR=rep(NA,5), other_lci=rep(NA,5), other_uci=rep(NA,5))

#weighted
for (i in 1:length(outcomes)){
  outcome=colnames(clean_data_hrqol)[i+8]
  model<-svyglm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), design=nhats_design_nodem, family=quasibinomial(link=log), start=c(-0.5,rep(0,9)))
  results_weighted_nodem$black_RR[i]<-exp(coef(model)[2])
  results_weighted_nodem$hispanic_RR[i]<-exp(coef(model)[3])
  results_weighted_nodem$other_RR[i]<-exp(coef(model)[4])
  results_weighted_nodem$black_lci[i]<-exp(confint(model)[2,1])
  results_weighted_nodem$black_uci[i]<-exp(confint(model)[2,2])
  results_weighted_nodem$hispanic_lci[i]<-exp(confint(model)[3,1])
  results_weighted_nodem$hispanic_uci[i]<-exp(confint(model)[3,2])
  results_weighted_nodem$other_lci[i]<-exp(confint(model)[4,1])
  results_weighted_nodem$other_uci[i]<-exp(confint(model)[4,2])
}

#unweighted
for (i in 1:length(outcomes)){
  outcome=colnames(clean_data_hrqol)[i+8]
  model<-glm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), data=clean_data_hrqol_nodem, family=binomial(link=log))
  results_unweighted_nodem$black_RR[i]<-exp(coef(model)[2])
  results_unweighted_nodem$hispanic_RR[i]<-exp(coef(model)[3])
  results_unweighted_nodem$other_RR[i]<-exp(coef(model)[4])
  results_unweighted_nodem$black_lci[i]<-exp(confint(model)[2,1])
  results_unweighted_nodem$black_uci[i]<-exp(confint(model)[2,2])
  results_unweighted_nodem$hispanic_lci[i]<-exp(confint(model)[3,1])
  results_unweighted_nodem$hispanic_uci[i]<-exp(confint(model)[3,2])
  results_unweighted_nodem$other_lci[i]<-exp(confint(model)[4,1])
  results_unweighted_nodem$other_uci[i]<-exp(confint(model)[4,2])
}

#------------------------------------------------------------------
# Export results #
#------------------------------------------------------------------

results_weighted$dementia<-1
results_weighted_nodem$dementia<-0

results_weighted_total<-rbind(results_weighted,results_weighted_nodem)

test<-gather(data=results_weighted_total, key="race", value="RR", black_RR, hispanic_RR, other_RR)
test$outcome2<-as.character(test$outcome)
test$outcome2[test$outcome=="funclimits"]<-">=1 ADL limitation"
test$outcome2[test$outcome=="pain.bother"]<-"Bothered by pain"
test$outcome2[test$outcome=="poorhealth.bin"]<-"Fair/poor health"
test$outcome2[test$outcome=="prob.dep"]<-"Screen+ depresson"
test$outcome2[test$outcome=="prob.anx"]<-"Screen+ anxiety"

test$race[test$race=="black_RR"] <- "Black vs. White"
test$race[test$race=="hispanic_RR"] <- "Latino vs. White"
test$race[test$race=="other_RR"] <- "Other vs. White"

test_black<-test[test$race=="Black vs. White",]
test_black$lci<-test_black$black_lci
test_black$uci<-test_black$black_uci
test_latino<-test[test$race=="Latino vs. White",]
test_latino$lci<-test_latino$hispanic_lci
test_latino$uci<-test_latino$hispanic_uci
test_other<-test[test$race=="Other vs. White",]
test_other$lci<-test_other$other_lci
test_other$uci<-test_other$other_uci

forplot<-rbind(test_black[,c("outcome2", "race", "RR", "lci", "uci", "dementia")],
               test_latino[,c("outcome2", "race", "RR", "lci", "uci", "dementia")],
               test_other[,c("outcome2", "race", "RR", "lci", "uci", "dementia")])


forplot$race<-factor(forplot$race, levels=c("Black vs. White", "Latino vs. White", "Other vs. White"), labels=c("Black vs. White", "Latino vs. White", "Other"))

results<-ggplot(data=test[test$race=="Black vs. White" | test$race=="Latino",], 
            aes(x=outcome2, y=RR, fill=as.factor(dementia), group=dementia))+
            geom_col(position="dodge")+ xlab("HRQOL indicator")+
            ylab("Prevalence ratio (95% CI)")+ facet_grid(.~race)+
            scale_fill_discrete(name="", labels=c("No dementia", "Dementia"))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90, size=12), 
                  axis.text.y = element_text(size=12), 
                  axis.title.x = element_text(size=14), 
                  axis.title.y = element_text(size=14), 
            )


results<-ggplot(data=forplot[forplot$race=="Black vs. White" | forplot$race=="Latino vs. White",])+
  geom_pointrange(aes(x=outcome2, y=RR, ymin=lci, ymax=uci, group=as.factor(dementia), color=as.factor(dementia)), position=position_dodge(width=0.5), size=1, shape=15)+
  xlab("HRQOL indicator")+ ylab("Prevalence ratio vs. white")+ facet_grid(.~race)+
  scale_color_manual(name="", labels=c("No dementia", "Dementia"), values=c("navy", "steelblue"))+
  theme_bw()+
  geom_hline(yintercept=1, colour="black", lwd=1) +
  theme(axis.text.x = element_text(angle = 70, hjust=1, size=12), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
  )


results
ggsave("C:/Users/ehlarson/Box/NHATS/OUTPUT/RRgraph.jpg", 
       plot=results,  dpi="retina")



#------------------------------------------------------------------
# Export results #
#------------------------------------------------------------------

results.list <- list("Unweighted_dem" = results_unweighted, "Unweighted_nodem" = results_unweighted_nodem, "Weighted_dem" = results_weighted, "Weighted_nodem" = results_weighted_nodem)
write.xlsx(results.list, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/RR_HRQOL_bydem.xlsx")
