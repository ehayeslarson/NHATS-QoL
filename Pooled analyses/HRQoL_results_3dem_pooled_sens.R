#------------------------------------------------------------------
# Title: HRQoL sensitivity analysis pooled
# Author: Eleanor Hayes-Larson
#------------------------------------------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "gee", "geepack",  
       "survey", "tableone", "openxlsx", "emmeans", "rlang")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")

clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

#creating indicator variables for age and race/ethnicity
clean_data_hrqol$black<-ifelse(clean_data_hrqol$race.eth==2,1,0)
clean_data_hrqol$latino<-ifelse(clean_data_hrqol$race.eth==3,1,0)

clean_data_hrqol$agecat2<-ifelse(as.numeric(clean_data_hrqol$age.cat)==2,1,0)
clean_data_hrqol$agecat3<-ifelse(as.numeric(clean_data_hrqol$age.cat)==3,1,0)
clean_data_hrqol$agecat4<-ifelse(as.numeric(clean_data_hrqol$age.cat)==4,1,0)
clean_data_hrqol$agecat5<-ifelse(as.numeric(clean_data_hrqol$age.cat)==5,1,0)
clean_data_hrqol$agecat6<-ifelse(as.numeric(clean_data_hrqol$age.cat)==6,1,0)

#saving dementa-free and dementia datasets
clean_data_hrqol_demprob<-clean_data_hrqol[clean_data_hrqol$dem_sens1==1,]
clean_data_hrqol_demposs<-clean_data_hrqol[clean_data_hrqol$dem_sens1==2,]
clean_data_hrqol_nodem<-clean_data_hrqol[clean_data_hrqol$dem_sens1==3,]


outcomes<-c("prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")


# ---Unweighted analyses--- #

#Relative risk regression models

unwghtedmod<-function(indata,linkfxn){
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5))
  
  models<-rep(list(NA),5)
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste0(outcome,"~black+latino+agecat2+agecat3+agecat4+agecat5+agecat6+female")),  
                  id=spid, data=indata, corstr="exchangeable", family=poisson(link=linkfxn))
    
    models[[i]]<-model
    
    for (j in 2:3){ 
      if (linkfxn=="log"){
        outresults[i,j] <- exp(coef(model)[j])
        outresults[i,j+2] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"])
        outresults[i,j+4] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"])
      } else if (linkfxn=="identity"){
        outresults[i,j] <- coef(model)[j]
        outresults[i,j+2] <- coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"]
        outresults[i,j+4] <- coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"]
      }
    }
  }
  return(list(outresults=outresults, models=models))
}


results_unweighted_demprob<-unwghtedmod(clean_data_hrqol_demprob, "log")      
results_unweighted_demposs<-unwghtedmod(clean_data_hrqol_demposs, "log")      
results_unweighted_nodem<-unwghtedmod(clean_data_hrqol_nodem, "log")
results_unweighted_demprob_RD<-unwghtedmod(clean_data_hrqol_demprob, "identity")
results_unweighted_demposs_RD<-unwghtedmod(clean_data_hrqol_demposs, "identity")
results_unweighted_nodem_RD<-unwghtedmod(clean_data_hrqol_nodem, "identity")      

# ---Weighted analyses--- #

wghtedmod<-function(dem,weightvar, linkfxn){ 
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5))
  
  models<-rep(list(NA),5)
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste0(outcome,"~black+latino+agecat2+agecat3+agecat4+agecat5+agecat6+female")),  
                  id=spid, data=clean_data_hrqol[clean_data_hrqol$dem_sens1==dem,], 
                  corstr="exchangeable", family=poisson(link=linkfxn), weights=weightvar[clean_data_hrqol$dem_sens1==dem])
    
    models[[i]]<-model
    
    for (j in 2:3){
      if (linkfxn=="log"){ 
        outresults[i,j] <- exp(coef(model)[j])
        outresults[i,j+2] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"])
        outresults[i,j+4] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"])
      } else if (linkfxn=="identity"){
        outresults[i,j] <- coef(model)[j]
        outresults[i,j+2] <- coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"]
        outresults[i,j+4] <- coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"]
      }
    }
  }
  return(list(outresults=outresults, models=models))
}

#RR models
results_weighted_bl_demprob<-wghtedmod(1, clean_data_hrqol$baseline.anwgt_scaled, "log")      
results_weighted_av_demprob<-wghtedmod(1, clean_data_hrqol$average.anwgt_scaled, "log")      
results_weighted_rd_demprob<-wghtedmod(1, clean_data_hrqol$analytic.wgt_scaled, "log")      

results_weighted_bl_demposs<-wghtedmod(2, clean_data_hrqol$baseline.anwgt_scaled, "log")      
results_weighted_av_demposs<-wghtedmod(2, clean_data_hrqol$average.anwgt_scaled, "log")      
results_weighted_rd_demposs<-wghtedmod(2, clean_data_hrqol$analytic.wgt_scaled, "log")      

results_weighted_bl_nodem<-wghtedmod(3, clean_data_hrqol$baseline.anwgt_scaled, "log")     
results_weighted_av_nodem<-wghtedmod(3, clean_data_hrqol$average.anwgt_scaled, "log")     
results_weighted_rd_nodem<-wghtedmod(3, clean_data_hrqol$analytic.wgt_scaled, "log")     

#RD models
results_weighted_bl_demprob_RD<-wghtedmod(1, clean_data_hrqol$baseline.anwgt_scaled, "identity")      
results_weighted_av_demprob_RD<-wghtedmod(1, clean_data_hrqol$average.anwgt_scaled, "identity")      
results_weighted_rd_demprob_RD<-wghtedmod(1, clean_data_hrqol$analytic.wgt_scaled, "identity")      

results_weighted_bl_demposs_RD<-wghtedmod(2, clean_data_hrqol$baseline.anwgt_scaled, "identity")      
results_weighted_av_demposs_RD<-wghtedmod(2, clean_data_hrqol$average.anwgt_scaled, "identity")      
results_weighted_rd_demposs_RD<-wghtedmod(2, clean_data_hrqol$analytic.wgt_scaled, "identity")      

results_weighted_bl_nodem_RD<-wghtedmod(3, clean_data_hrqol$baseline.anwgt_scaled, "identity")     
results_weighted_av_nodem_RD<-wghtedmod(3, clean_data_hrqol$average.anwgt_scaled, "identity")     
results_weighted_rd_nodem_RD<-wghtedmod(3, clean_data_hrqol$analytic.wgt_scaled, "identity")     


#------------------------------------------------------------------
# Format and export results #
#------------------------------------------------------------------

res_tbls<-list(results_unweighted_demprob=results_unweighted_demprob$outresults, 
               results_unweighted_demposs=results_unweighted_demposs$outresults, 
               results_unweighted_nodem=results_unweighted_nodem$outresults,
               
               results_weighted_bl_demprob=results_weighted_bl_demprob$outresults,
               results_weighted_av_demprob=results_weighted_av_demprob$outresults,
               results_weighted_rd_demprob=results_weighted_rd_demprob$outresults,
               
               results_weighted_bl_demposs=results_weighted_bl_demposs$outresults,
               results_weighted_av_demposs=results_weighted_av_demposs$outresults,
               results_weighted_rd_demposs=results_weighted_rd_demposs$outresults,
               
               results_weighted_bl_nodem=results_weighted_bl_nodem$outresults,
               results_weighted_av_nodem=results_weighted_av_nodem$outresults,
               results_weighted_rd_nodem=results_weighted_rd_nodem$outresults,
               
               results_unweighted_demprob_RD=results_unweighted_demprob_RD$outresults, 
               results_unweighted_demposs_RD=results_unweighted_demposs_RD$outresults, 
               results_unweighted_nodem_RD=results_unweighted_nodem_RD$outresults,
               
               results_weighted_bl_demprob_RD=results_weighted_bl_demprob_RD$outresults,
               results_weighted_av_demprob_RD=results_weighted_av_demprob_RD$outresults,
               results_weighted_rd_demprob_RD=results_weighted_rd_demprob_RD$outresults,
               
               results_weighted_bl_demposs_RD=results_weighted_bl_demposs_RD$outresults,
               results_weighted_av_demposs_RD=results_weighted_av_demposs_RD$outresults,
               results_weighted_rd_demposs_RD=results_weighted_rd_demposs_RD$outresults,
               
               results_weighted_bl_nodem_RD=results_weighted_bl_nodem_RD$outresults,
               results_weighted_av_nodem_RD=results_weighted_av_nodem_RD$outresults,
               results_weighted_rd_nodem_RD=results_weighted_rd_nodem_RD$outresults)


for (i in 1:length(res_tbls) ){
  
  name<-names(res_tbls)[i]
  
  res_tbls[[i]]$dementia<-ifelse(grepl("nodem", substr(name, 0,100000)),3,ifelse(grepl("demposs", substr(name, 0,100000)), 2, 1))
  res_tbls[[i]]$weight<-paste(substr(name, 9, 19))
  res_tbls[[i]]$measure<-ifelse(grepl("RD", substr(name, 0,100000)),"RD","RR")
  
}

results_all<-do.call(rbind,res_tbls)
rownames(results_all)<-NULL

save(results_all,file="C:/Users/ehlarson/Box/NHATS/OUTPUT/HRQOL_pooled_sens.Rdata")
write.xlsx(res_tbls, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/HRQOL_pooled_sens.xlsx")


#------------------------------------------------------------------
# Standardized predicted prevalences
#------------------------------------------------------------------

pred_prevs<-function(modelvar){
  
  outset<-data.frame(outcome=outcomes, white_est=rep(NA,5), white_LCI=rep(NA,5), white_UCI=rep(NA,5), 
                     black_est=rep(NA,5), black_LCI=rep(NA,5), black_UCI=rep(NA,5), 
                     latino_est=rep(NA,5), latino_LCI=rep(NA,5), latino_UCI=rep(NA,5)) 

  
  for (i in 1:length(outcomes)){

    pred_p<-tidy(emmeans(modelvar[[2]][[i]], specs=c("black", "latino"), weights = "proportional"))
    
    outset$white_est[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==0])  
    outset$white_LCI[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==0]-1.96*pred_p$std.error[pred_p$black==0 & pred_p$latino==0])
    outset$white_UCI[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==0]+1.96*pred_p$std.error[pred_p$black==0 & pred_p$latino==0])
    
    outset$black_est[i]<-exp(pred_p$estimate[pred_p$black==1 & pred_p$latino==0])
    outset$black_LCI[i]<-exp(pred_p$estimate[pred_p$black==1 & pred_p$latino==0]-1.96*pred_p$std.error[pred_p$black==1 & pred_p$latino==0])
    outset$black_UCI[i]<-exp(pred_p$estimate[pred_p$black==1 & pred_p$latino==0]+1.96*pred_p$std.error[pred_p$black==1 & pred_p$latino==0])
    
    outset$latino_est[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==1])
    outset$latino_LCI[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==1]-1.96*pred_p$std.error[pred_p$black==0 & pred_p$latino==1])
    outset$latino_UCI[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==1]+1.96*pred_p$std.error[pred_p$black==0 & pred_p$latino==1])
    
  }
  
  return(outset)
}

pred_prev_demprob<-pred_prevs(results_weighted_bl_demprob)
pred_prev_demposs<-pred_prevs(results_weighted_bl_demposs)
pred_prev_nodem<-pred_prevs(results_weighted_bl_nodem)

pred_prev_demprob$dementia<-1
pred_prev_demposs$dementia<-2
pred_prev_nodem$dementia<-3


pred_all<-rbind(pred_prev_demprob, pred_prev_demposs, pred_prev_nodem)


pred_list<-list(pred_prev_demprob=pred_prev_demprob,
                pred_prev_demposs=pred_prev_demposs, 
                pred_prev_nodem=pred_prev_nodem)

save(pred_all,file="C:/Users/ehlarson/Box/NHATS/OUTPUT/predicted_pooled_sens.Rdata")
write.xlsx(pred_list, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/predicted_pooled_sens.xlsx")



