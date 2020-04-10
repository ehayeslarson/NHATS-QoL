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
       "survey", "tableone", "openxlsx", "emmeans", "rlang")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")
clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

#creating indicator variables for age and race/ethnicity
clean_data_hrqol$black<-ifelse(clean_data_hrqol$race.eth==2,1,0)
clean_data_hrqol$latino<-ifelse(clean_data_hrqol$race.eth==3,1,0)
clean_data_hrqol$other<-ifelse(clean_data_hrqol$race.eth==4,1,0)

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
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  models<-rep(list(NA),5)
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste0(outcome,"~black+latino+other+agecat2+agecat3+agecat4+agecat5+agecat6+female")), 
                  id=spid, data=indata, corstr="exchangeable", family=poisson(link=linkfxn))
    
    models[[i]]<-model
    
    for (j in 2:4){
      if (linkfxn=="log"){
        outresults[i,j] <- exp(coef(model)[j])
        outresults[i,j+3] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"])
        outresults[i,j+6] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"])
      } else if (linkfxn=="identity"){
        outresults[i,j] <- coef(model)[j]
        outresults[i,j+3] <- coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"]
        outresults[i,j+6] <- coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"]
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
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  models<-rep(list(NA),5)
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste0(outcome,"~black+latino+other+agecat2+agecat3+agecat4+agecat5+agecat6+female")), 
                  id=spid, data=clean_data_hrqol[clean_data_hrqol$dem_sens1==dem,], 
                  corstr="exchangeable", family=poisson(link=linkfxn), weights=weightvar[clean_data_hrqol$dem_sens1==dem])
    
    models[[i]]<-model
    
    for (j in 2:4){
      if (linkfxn=="log"){
        outresults[i,j] <- exp(coef(model)[j])
        outresults[i,j+3] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"])
        outresults[i,j+6] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"])
      } else if (linkfxn=="identity"){
        outresults[i,j] <- coef(model)[j]
        outresults[i,j+3] <- coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"]
        outresults[i,j+6] <- coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"]
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
results_forplot<-pivot_longer(results_all, cols = c("black_est", "hispanic_est", "other_est"),
                              names_to="race")

results_forplot$LCI<-ifelse(results_forplot$race=="black_est", results_forplot$black_lci, 
                            ifelse(results_forplot$race=="hispanic_est", results_forplot$hispanic_lci,results_forplot$other_lci))

results_forplot$UCI<-ifelse(results_forplot$race=="black_est", results_forplot$black_uci, 
                            ifelse(results_forplot$race=="hispanic_est", results_forplot$hispanic_uci,results_forplot$other_uci))

results_forplot<- results_forplot[,c("outcome","race", "dementia", "weight", "measure", "value", "LCI", "UCI")]


results_forplot$outcome2<-NA
results_forplot$outcome2[results_forplot$outcome=="funclimits"]<-"Functional limitations"
results_forplot$outcome2[results_forplot$outcome=="pain.bother"]<-"Bothered by pain"
results_forplot$outcome2[results_forplot$outcome=="poorhealth.bin"]<-"Fair/poor health"
results_forplot$outcome2[results_forplot$outcome=="prob.dep"]<-"Elevated depressive \nsymptoms"
results_forplot$outcome2[results_forplot$outcome=="prob.anx"]<-"Elevated anxiety \nsymptoms"

results_forplot$race[results_forplot$race=="black_est"] <- "Black vs. White"
results_forplot$race[results_forplot$race=="hispanic_est"] <- "Latino vs. White"
results_forplot$race[results_forplot$race=="other_est"] <- "Other vs. White"





plotresRR<-function(wt){
  ggplot(data=results_forplot[(results_forplot$race=="Black vs. White" | 
                                 results_forplot$race=="Latino vs. White") & 
                                results_forplot$measure=="RR" &
                                results_forplot$weight==wt,])+
    geom_pointrange(aes(x=factor(outcome2, levels = c("Bothered by pain", "Elevated anxiety \nsymptoms",
                                                      "Functional limitations", "Elevated depressive \nsymptoms",
                                                      "Fair/poor health")), y=value, ymin=LCI, ymax=UCI, group=factor(dementia, levels=c(3,2,1)), 
                        color=factor(dementia, levels=c(1,2,3))), position=position_dodge(width=0.6), size=1, shape=15)+
    xlab("")+ ylab("Prevalence ratio (95% CI)")+ ylim(0.7,2.6)+facet_grid(.~race)+
    scale_color_manual(name="", breaks=c(1,2,3),
                       labels=c("Probable dementia", "Possible dementia","No dementia"), 
                       values=c("tan4", "tan3", "tan1"))+
    theme_bw()+ 
    guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
    geom_hline(yintercept=1, colour="black", lwd=1) +
    theme(axis.text.x = element_text(size=12), 
          axis.text.y = element_text(size=12), 
          axis.title.x = element_text(size=14), 
          axis.title.y = element_text(size=14), 
          legend.position = "bottom"
    )+ coord_flip()
}


res_un_RR<-plotresRR("unweighted_")
res_bl_RR<-plotresRR("weighted_bl")
res_av_RR<-plotresRR("weighted_av")


plotresRD<-function(wt){
  ggplot(data=results_forplot[(results_forplot$race=="Black vs. White" | 
                                 results_forplot$race=="Latino vs. White") & 
                                results_forplot$measure=="RD" &
                                results_forplot$weight==wt,])+
    geom_pointrange(aes(x=factor(outcome2, levels = c("Bothered by pain", "Elevated anxiety \nsymptoms",
                                                      "Functional limitations", "Elevated depressive \nsymptoms",
                                                      "Fair/poor health")), y=value, ymin=LCI, ymax=UCI, group=factor(dementia, levels=c(3,2,1)), 
                        color=factor(dementia, levels=c(1,2,3))), position=position_dodge(width=0.6), size=1, shape=15)+
    xlab("")+ ylab("Prevalence difference (95% CI)")+ facet_grid(.~race)+
    scale_color_manual(name="", breaks=c(1,2,3),
                       labels=c("Probable dementia", "Possible dementia","No dementia"), 
                       values=c("tan4", "tan3", "tan1"))+
    theme_bw()+ scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.15,0.3))+
    guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
    geom_hline(yintercept=0, colour="black", lwd=1) +
    theme(axis.text.x = element_text(size=12), 
          axis.text.y = element_text(size=12), 
          axis.title.x = element_text(size=14), 
          axis.title.y = element_text(size=14), 
          legend.position = "bottom"
    )+ coord_flip()
}


res_un_RD<-plotresRD("unweighted_")
res_bl_RD<-plotresRD("weighted_bl")
res_av_RD<-plotresRD("weighted_av")


figures<-list(res_un_RR=res_un_RR, res_av_RR=res_av_RR, res_bl_RR=res_bl_RR, 
              res_un_RD=res_un_RD, res_av_RD=res_av_RD, res_bl_RD=res_bl_RD)

for (i in 1:length(figures)){
  ggsave(filename=paste0("C:/Users/ehlarson/Box/NHATS/OUTPUT/FIGURES/SENS/",names(figures)[i],"_sens.jpg"), 
         plot=eval(parse_expr(names(figures[i]))), dpi="retina", width=6.5)
}


save(results_all,file="C:/Users/ehlarson/Box/NHATS/OUTPUT/HRQOL_pooled_sens.Rdata")
write.xlsx(res_tbls, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/HRQOL_pooled_sens.xlsx")



#------------------------------------------------------------------
# Standardized predicted prevalences
#------------------------------------------------------------------

pred_prevs<-function(modelvar){
  
  outset<-data.frame(outcome=outcomes, white_est=rep(NA,5), white_LCI=rep(NA,5), white_UCI=rep(NA,5), 
                     black_est=rep(NA,5), black_LCI=rep(NA,5), black_UCI=rep(NA,5), 
                     latino_est=rep(NA,5), latino_LCI=rep(NA,5), latino_UCI=rep(NA,5), 
                     other_est=rep(NA,5), other_LCI=rep(NA,5), other_UCI=rep(NA,5))
  
  
  for (i in 1:length(outcomes)){
    
    
    pred_p<-tidy(emmeans(modelvar[[2]][[i]], specs=c("black", "latino", "other"), weights = "proportional"))
    
    outset$white_est[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==0 & pred_p$other==0])
    outset$white_LCI[i]<-exp(pred_p$asymp.LCL[pred_p$black==0 & pred_p$latino==0 & pred_p$other==0])
    outset$white_UCI[i]<-exp(pred_p$asymp.UCL[pred_p$black==0 & pred_p$latino==0 & pred_p$other==0])
    
    outset$black_est[i]<-exp(pred_p$estimate[pred_p$black==1 & pred_p$latino==0 & pred_p$other==0])
    outset$black_LCI[i]<-exp(pred_p$asymp.LCL[pred_p$black==1 & pred_p$latino==0 & pred_p$other==0])
    outset$black_UCI[i]<-exp(pred_p$asymp.UCL[pred_p$black==1 & pred_p$latino==0 & pred_p$other==0])
    
    outset$latino_est[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==1 & pred_p$other==0])
    outset$latino_LCI[i]<-exp(pred_p$asymp.LCL[pred_p$black==0 & pred_p$latino==1 & pred_p$other==0])
    outset$latino_UCI[i]<-exp(pred_p$asymp.UCL[pred_p$black==0 & pred_p$latino==1 & pred_p$other==0])
    
    outset$other_est[i]<-exp(pred_p$estimate[pred_p$black==0 & pred_p$latino==0 & pred_p$other==1])
    outset$other_LCI[i]<-exp(pred_p$asymp.LCL[pred_p$black==0 & pred_p$latino==0 & pred_p$other==1])
    outset$other_UCI[i]<-exp(pred_p$asymp.UCL[pred_p$black==0 & pred_p$latino==0 & pred_p$other==1])
    
  }
  
  return(outset)
}

pred_prev_demprob<-pred_prevs(results_weighted_bl_demprob)
pred_prev_demposs<-pred_prevs(results_weighted_bl_demposs)
pred_prev_nodem<-pred_prevs(results_weighted_bl_nodem)

pred_prev_demprob$dementia<-1
pred_prev_demposs$dementia<-2
pred_prev_nodem$dementia<-3


pred_all<-rbind(pred_prev_demprob,pred_prev_demposs, pred_prev_nodem)

pred_forplot<-pivot_longer(pred_all, cols = c("white_est", "black_est", "latino_est", "other_est"),
                           names_to="race")

pred_forplot$LCI<-ifelse(pred_forplot$race=="white_est", pred_forplot$white_LCI,
                         ifelse(pred_forplot$race=="black_est", pred_forplot$black_LCI, 
                                ifelse(pred_forplot$race=="latino_est", pred_forplot$latino_LCI,pred_forplot$other_LCI)))

pred_forplot$UCI<-ifelse(pred_forplot$race=="white_est", pred_forplot$white_UCI,
                         ifelse(pred_forplot$race=="black_est", pred_forplot$black_UCI, 
                                ifelse(pred_forplot$race=="latino_est", pred_forplot$latino_UCI,pred_forplot$other_UCI)))


pred_forplot<- pred_forplot[,c("outcome","race", "dementia", "value", "LCI", "UCI")]


pred_forplot$outcome2<-NA
pred_forplot$outcome2[pred_forplot$outcome=="funclimits"]<-"Functional limitations"
pred_forplot$outcome2[pred_forplot$outcome=="pain.bother"]<-"Bothered by pain"
pred_forplot$outcome2[pred_forplot$outcome=="poorhealth.bin"]<-"Fair/poor health"
pred_forplot$outcome2[pred_forplot$outcome=="prob.dep"]<-"Elevated depressive symptoms"
pred_forplot$outcome2[pred_forplot$outcome=="prob.anx"]<-"Elevated anxiety symptoms"

pred_forplot$outcome2<-factor(pred_forplot$outcome2, levels = c("Fair/poor health", "Elevated depressive symptoms",
                                                                "Functional limitations", "Elevated anxiety symptoms",
                                                                "Bothered by pain"))

pred_forplot$race[pred_forplot$race=="white_est"] <- "White"
pred_forplot$race[pred_forplot$race=="black_est"] <- "Black"
pred_forplot$race[pred_forplot$race=="latino_est"] <- "Latino"
pred_forplot$race[pred_forplot$race=="other_est"] <- "Other"


pred_prev_blwt_dem_byrace<-ggplot(data=pred_forplot[pred_forplot$race %in% c("White", "Black", "Latino"),])+
  geom_col(aes(x=race, y=value, group=factor(dementia), 
               fill=factor(dementia)), width=0.75, position=position_dodge(width=.75))+
  geom_errorbar(aes(x=race, ymin=LCI, ymax=UCI, group=factor(dementia)), width=0.5, position=position_dodge(width=0.75))+
  xlab(NULL)+ ylab("Standardized predicted prevalence (95% CI)")+ 
  facet_wrap( ~ outcome2, ncol=1)+
  scale_fill_manual(name="", labels=c("Probable dementia", "Possible dementia", "No dementia"), values=c("tan4", "tan3", "tan1"))+
  theme_bw()+ scale_y_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,0.75))+
  geom_hline(yintercept=0, colour="black", lwd=0.5) +
  theme(axis.text.x = element_text(size=14), 
        axis.text.y = element_text(size=14), 
        axis.title.x = element_text(size=16), 
        axis.title.y = element_text(size=16),
        strip.text.x = element_text(size = 12),
        legend.position = "bottom"
  )



pred_prev_blwt_dem_byrace

ggsave(filename=paste0("C:/Users/ehlarson/Box/NHATS/OUTPUT/FIGURES/SENS/pred_prev_blwt_dem_byrace_sens.jpg"), 
       plot=pred_prev_blwt_dem_byrace, dpi="retina", width=10)




