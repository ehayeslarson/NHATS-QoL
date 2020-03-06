#------------------------------------------------------------------
# Title: HRQoL main analysis pooled
# Author: Eleanor Hayes-Larson
#------------------------------------------------------------------

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

unwghtedmod<-function(indata){
        outresults<-data.frame(outcome=outcomes,
                               black_RR=rep(NA,5), hispanic_RR=rep(NA,5), other_RR=rep(NA,5), 
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
      

      results_unweighted_dem<-unwghtedmod(clean_data_hrqol_dem)      
      results_unweighted_nodem<-unwghtedmod(clean_data_hrqol_nodem)      
      
      
# ---Weighted analyses--- #



wghtedmod<-function(dem,weightvar){
  outresults<-data.frame(outcome=outcomes,
                         black_RR=rep(NA,5), hispanic_RR=rep(NA,5), other_RR=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), 
               id=spid, data=clean_data_hrqol[clean_data_hrqol$dementia.bin==dem,], 
               corstr="exchangeable", family=poisson(link=log), weights=weightvar[clean_data_hrqol$dementia.bin==dem])
    
    for (j in 2:4){
      outresults[i,j] <- exp(coef(model)[j])
      outresults[i,j+3] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"])
      outresults[i,j+6] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"])
    }
  }
  return(outresults)
}


results_weighted_bl_dem<-wghtedmod(1, clean_data_hrqol$baseline.anwgt)      
results_weighted_av_dem<-wghtedmod(1, clean_data_hrqol$average.anwgt)      
results_weighted_rd_dem<-wghtedmod(1, clean_data_hrqol$analytic.wgt)      

results_weighted_bl_nodem<-wghtedmod(0, clean_data_hrqol$baseline.anwgt)     
results_weighted_av_nodem<-wghtedmod(0, clean_data_hrqol$average.anwgt)     
results_weighted_rd_nodem<-wghtedmod(0, clean_data_hrqol$analytic.wgt)     



#------------------------------------------------------------------
# Format and export results #
#------------------------------------------------------------------

res_tbls<-list(results_unweighted_dem=results_unweighted_dem, 
               results_unweighted_nodem=results_unweighted_nodem,
               results_weighted_bl_dem=results_weighted_bl_dem,
               results_weighted_av_dem=results_weighted_av_dem,
               results_weighted_rd_dem=results_weighted_rd_dem,
               results_weighted_bl_nodem=results_weighted_bl_nodem,
               results_weighted_av_nodem=results_weighted_av_nodem,
               results_weighted_rd_nodem=results_weighted_rd_nodem)


  for (i in 1:length(res_tbls) ){
  
  name<-names(res_tbls)[i]
  
  res_tbls[[i]][,"dementia"]<-ifelse(grepl("nodem", substr(name, 0,100000)),0,1)
  res_tbls[[i]][,"weight"]<-paste(substr(name, 9, 19))
  
  }
  

results_all<-do.call(rbind,res_tbls)


save(results_all,file="C:/Users/ehlarson/Box/NHATS/OUTPUT/RR_HRQOL_pooled.Rdata")
write.xlsx(results_all, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/RR_HRQOL_pooled.xlsx")
