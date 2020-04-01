#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "gee", "geepack",  
       "survey", "tableone", "openxlsx")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")
clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]
clean_data_hrqol_dem<-clean_data[clean_data$comp.case.HRQoL==1 & clean_data$dementia.bin==1,]
clean_data_hrqol_nodem<-clean_data[clean_data$comp.case.HRQoL==1 & clean_data$dementia.bin==0,]
outcomes<-c("prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")

# ---Unweighted analyses--- #

#Relative risk regression models

unwghtedmod_bin<-function(indata){
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), 
                  id=spid, data=indata, corstr="exchangeable", family=binomial(link=log))
    
    for (j in 2:4){
      outresults[i,j] <- exp(coef(model)[j])
      outresults[i,j+3] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"])
      outresults[i,j+6] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"])
    }
  }
  return(outresults)
}


results_unweighted_dem_bin<-unwghtedmod_bin(clean_data_hrqol_dem)      
results_unweighted_nodem_bin<-unwghtedmod_bin(clean_data_hrqol_nodem) 



unwghtedmod_pois<-function(indata){
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), 
                  id=spid, data=indata, corstr="exchangeable", family=poisson(link=log))
    
    for (j in 2:4){
      outresults[i,j] <- exp(coef(model)[j])
      outresults[i,j+3] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"])
      outresults[i,j+6] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"])
    }
  }
  return(outresults)
}


results_unweighted_dem_pois<-unwghtedmod_pois(clean_data_hrqol_dem)      
results_unweighted_nodem_pois<-unwghtedmod_pois(clean_data_hrqol_nodem) 



res_tbls<-list(results_unweighted_dem_bin=results_unweighted_dem_bin, 
               results_unweighted_nodem_bin=results_unweighted_nodem_bin,

               results_unweighted_dem_pois=results_unweighted_dem_pois, 
               results_unweighted_nodem_pois=results_unweighted_nodem_pois)

for (i in 1:length(res_tbls) ){
  
  name<-names(res_tbls)[i]
  
  res_tbls[[i]][,"dementia"]<-ifelse(grepl("nodem", substr(name, 0,100000)),"No dementia","Dementia")
  res_tbls[[i]][,"dist"]<-ifelse(grepl("bin", substr(name, 0,100000)),"Binomial","Poisson")

}


results_all<-do.call(rbind, res_tbls)

test<-pivot_longer(results_all, cols = c("black_est", "hispanic_est", "other_est"),
                   names_to="race")

test$LCI<-ifelse(test$race=="black_est", test$black_lci, 
                  ifelse(test$race=="hispanic_est", test$hispanic_lci,test$other_lci))

test$UCI<-ifelse(test$race=="black_est", test$black_uci, 
                 ifelse(test$race=="hispanic_est", test$hispanic_uci,test$other_uci))

test<- test[,c("outcome","race", "dementia", "dist", "value", "LCI", "UCI")]


test$outcome2<-NA
test$outcome2[test$outcome=="funclimits"]<-">=1 ADL limitation"
test$outcome2[test$outcome=="pain.bother"]<-"Bothered by pain"
test$outcome2[test$outcome=="poorhealth.bin"]<-"Fair/poor health"
test$outcome2[test$outcome=="prob.dep"]<-"Screen+ depresson"
test$outcome2[test$outcome=="prob.anx"]<-"Screen+ anxiety"

test$race[test$race=="black_est"] <- "Black vs. White"
test$race[test$race=="hispanic_est"] <- "Latino vs. White"
test$race[test$race=="other_est"] <- "Other vs. White"




results<-ggplot(data=test[test$race=="Black vs. White" | test$race=="Latino vs. White",])+
  geom_pointrange(aes(x=outcome2, y=value, ymin=LCI, ymax=UCI, group=as.factor(dist), 
                      color=as.factor(dist)), position=position_dodge(width=0.6), size=1, shape=15)+
  xlab("HRQOL indicator")+ ylab("Prevalence ratio (95% CI)")+ facet_grid(dementia~.~race)+
  scale_color_manual(name="", labels=c("Binomial", "Poisson"), values=c("red", "orange"))+
  theme_bw()+
  geom_hline(yintercept=1, colour="black", lwd=1) +
  theme(axis.text.x = element_text(angle = 70, hjust=1, size=12), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
  )
results
