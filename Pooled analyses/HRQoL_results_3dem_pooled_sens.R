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

#TMM loading data for code check
load("C:/Users/tmobley/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")

clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

#creating indicator variables for age and race/ethnicity
clean_data_hrqol$black<-ifelse(clean_data_hrqol$race.eth==2,1,0)
clean_data_hrqol$latino<-ifelse(clean_data_hrqol$race.eth==3,1,0)
#POST CODE REVIEW: Removed other indicator variable creation

clean_data_hrqol$agecat2<-ifelse(as.numeric(clean_data_hrqol$age.cat)==2,1,0)
clean_data_hrqol$agecat3<-ifelse(as.numeric(clean_data_hrqol$age.cat)==3,1,0)
clean_data_hrqol$agecat4<-ifelse(as.numeric(clean_data_hrqol$age.cat)==4,1,0)
clean_data_hrqol$agecat5<-ifelse(as.numeric(clean_data_hrqol$age.cat)==5,1,0)
clean_data_hrqol$agecat6<-ifelse(as.numeric(clean_data_hrqol$age.cat)==6,1,0)

#code checking
table(clean_data_hrqol$race.eth, exclude=NULL)
colSums(clean_data_hrqol[, c("black", "latino")])
table(clean_data_hrqol$age.cat, exclude=NULL)
colSums(clean_data_hrqol[, c("agecat2", "agecat3", "agecat4", "agecat5", "agecat6")])
#end check

#saving dementa-free and dementia datasets
clean_data_hrqol_demprob<-clean_data_hrqol[clean_data_hrqol$dem_sens1==1,]
clean_data_hrqol_demposs<-clean_data_hrqol[clean_data_hrqol$dem_sens1==2,]
clean_data_hrqol_nodem<-clean_data_hrqol[clean_data_hrqol$dem_sens1==3,]

#code checking
table(clean_data_hrqol$dem_sens1, useNA = "ifany")
table(clean_data_hrqol_demprob$dem_sens1)
table(clean_data_hrqol_demposs$dem_sens1)
table(clean_data_hrqol_nodem$dem_sens1)

outcomes<-c("prob.dep", "prob.anx", "poorhealth.bin", "pain.bother", "funclimits")


# ---Unweighted analyses--- #

#Relative risk regression models

unwghtedmod<-function(indata,linkfxn){
  outresults<-data.frame(outcome=outcomes, #POST CODE REVIEW: removed "other" est, lci, uci from next 3 lines.
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5))
  
  models<-rep(list(NA),5)
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste0(outcome,"~black+latino+agecat2+agecat3+agecat4+agecat5+agecat6+female")),  #POST CODE REVIEW: removed "other" indep var
                  id=spid, data=indata, corstr="exchangeable", family=poisson(link=linkfxn))
    
    models[[i]]<-model
    
    for (j in 2:3){ #POST CODE REVIEW: changed j loop to 2-3, results storing in j+2 and j+4 instead of 2-4, j+3, j+6.
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

#code checking
View(results_unweighted_demprob)
results_unweighted_demprob$outresults

# ---Weighted analyses--- #

wghtedmod<-function(dem,weightvar, linkfxn){ #POST CODE REVIEW: removed "other" est, lci, uci from next 3 lines.
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5))
  
  models<-rep(list(NA),5)
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste0(outcome,"~black+latino+agecat2+agecat3+agecat4+agecat5+agecat6+female")),  #POST CODE REVIEW: removed "other" indep var
                  id=spid, data=clean_data_hrqol[clean_data_hrqol$dem_sens1==dem,], 
                  corstr="exchangeable", family=poisson(link=linkfxn), weights=weightvar[clean_data_hrqol$dem_sens1==dem])
    
    models[[i]]<-model
    
    for (j in 2:3){
      if (linkfxn=="log"){ #POST CODE REVIEW: changed j loop to 2-3, results storing in j+2 and j+4 instead of 2-4, j+3, j+6.
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

#code checking
View(results_weighted_bl_demprob)
View(results_weighted_av_demprob_RD)
results_weighted_bl_demprob$outresults


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

#code check
View(res_tbls)

for (i in 1:length(res_tbls) ){
  
  name<-names(res_tbls)[i]
  
  res_tbls[[i]]$dementia<-ifelse(grepl("nodem", substr(name, 0,100000)),3,ifelse(grepl("demposs", substr(name, 0,100000)), 2, 1))
  res_tbls[[i]]$weight<-paste(substr(name, 9, 19))
  res_tbls[[i]]$measure<-ifelse(grepl("RD", substr(name, 0,100000)),"RD","RR")
  
}

#code checking
i=1 
name<-names(res_tbls)[i]

res_tbls[[i]]$dementia<-ifelse(grepl("nodem", substr(name, 0,100000)),3,ifelse(grepl("demposs", substr(name, 0,100000)), 2, 1))
res_tbls[[i]]$weight<-paste(substr(name, 9, 19))
res_tbls[[i]]$measure<-ifelse(grepl("RD", substr(name, 0,100000)),"RD","RR")
#end check


results_all<-do.call(rbind,res_tbls)
rownames(results_all)<-NULL

#code checking -- compare results_all and results_forplot
View(results_forplt)
View(results_all)

#code checking
plotresRR("unweighted_")
plotresRR("weighted_bl")
plotresRR("weighted_av")
#end check

#check code
View(figures)

save(results_all,file="C:/Users/ehlarson/Box/NHATS/OUTPUT/HRQOL_pooled_sens.Rdata")
write.xlsx(res_tbls, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/HRQOL_pooled_sens.xlsx")

#code checking -- save output to compare
for (i in 1:length(figures)){
  ggsave(filename=paste0("C:/Users/tmobley/Desktop/Git_Repos/NHATS-QoL/Output/",names(figures)[i],"_sens.jpg"), 
         plot=eval(parse_expr(names(figures[i]))), dpi="retina", width=6.5)
}

save(results_all,file="C:/Users/tmobley/Desktop/Git_Repos/NHATS-QoL/Output/HRQOL_pooled_sens.Rdata")
write.xlsx(res_tbls, file = "C:/Users/tmobley/Desktop/Git_Repos/NHATS-QoL/Output/HRQOL_pooled_sens.xlsx")
#end check

#------------------------------------------------------------------
# Standardized predicted prevalences
#------------------------------------------------------------------

pred_prevs<-function(modelvar){
  
  outset<-data.frame(outcome=outcomes, white_est=rep(NA,5), white_LCI=rep(NA,5), white_UCI=rep(NA,5), 
                     black_est=rep(NA,5), black_LCI=rep(NA,5), black_UCI=rep(NA,5), 
                     latino_est=rep(NA,5), latino_LCI=rep(NA,5), latino_UCI=rep(NA,5)) 
                     #POST CODE REVIEW: removed others
  
  
  for (i in 1:length(outcomes)){
    
    #POST CODE REVIEW: removed others from rows 220-236
    pred_p<-tidy(emmeans(modelvar[[2]][[i]], specs=c("black", "latino"), weights = "proportional"))
    
    outset$white_est[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==0])
    outset$white_LCI[i]<-exp(pred_p$asymp.LCL[pred_p$black==0 & pred_p$latino==0])
    outset$white_UCI[i]<-exp(pred_p$asymp.UCL[pred_p$black==0 & pred_p$latino==0])
    
    outset$black_est[i]<-exp(pred_p$estimate[pred_p$black==1 & pred_p$latino==0])
    outset$black_LCI[i]<-exp(pred_p$asymp.LCL[pred_p$black==1 & pred_p$latino==0])
    outset$black_UCI[i]<-exp(pred_p$asymp.UCL[pred_p$black==1 & pred_p$latino==0])
    
    outset$latino_est[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==1])
    outset$latino_LCI[i]<-exp(pred_p$asymp.LCL[pred_p$black==0 & pred_p$latino==1])
    outset$latino_UCI[i]<-exp(pred_p$asymp.UCL[pred_p$black==0 & pred_p$latino==1])
    
    outset$other_est[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==0])
    outset$other_LCI[i]<-exp(pred_p$asymp.LCL[pred_p$black==0 & pred_p$latino==0])
    outset$other_UCI[i]<-exp(pred_p$asymp.UCL[pred_p$black==0 & pred_p$latino==0])
    
  }
  
  return(outset)
}

pred_prev_demprob<-pred_prevs(results_weighted_bl_demprob)
pred_prev_demposs<-pred_prevs(results_weighted_bl_demposs)
pred_prev_nodem<-pred_prevs(results_weighted_bl_nodem)

#code checking
View(results_weighted_bl_demposs)
View(pred_prev_demprob)
#end check

pred_prev_demprob$dementia<-1
pred_prev_demposs$dementia<-2
pred_prev_nodem$dementia<-3

#code checking
View(pred_all)
View(pred_forplot)

pred_all<-rbind(pred_prev_demprob, pred_prev_demposs, pred_prev_nodem)


pred_list<-list(pred_prev_demprob=pred_prev_demprob,
                pred_prev_demposs=pred_prev_demposs, 
                pred_prev_nodem=pred_prev_nodem)

save(pred_all,file="C:/Users/ehlarson/Box/NHATS/OUTPUT/predicted_pooled_sens.Rdata")
write.xlsx(pred_list, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/predicted_pooled_sens.xlsx")





#code checking -- save output to local output computer
ggsave(filename=paste0("C:/Users/tmobley/Desktop/Git_Repos/NHATS-QoL/Output/pred_prev_blwt_dem_byrace_sens.jpg"), 
       plot=pred_prev_blwt_dem_byrace, dpi="retina", width=5, height=9)




