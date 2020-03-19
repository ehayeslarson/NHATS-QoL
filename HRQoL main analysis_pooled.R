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
clean_data_hrqol_dem<-clean_data_hrqol[clean_data_hrqol$dementia.bin==1,]
clean_data_hrqol_nodem<-clean_data_hrqol[clean_data_hrqol$dementia.bin==0,]


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
      

      results_unweighted_dem<-unwghtedmod(clean_data_hrqol_dem, "log")      
      results_unweighted_nodem<-unwghtedmod(clean_data_hrqol_nodem, "log")
      results_unweighted_dem_RD<-unwghtedmod(clean_data_hrqol_dem, "identity")      
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
                  id=spid, data=clean_data_hrqol[clean_data_hrqol$dementia.bin==dem,], 
               corstr="exchangeable", family=poisson(link=linkfxn), weights=weightvar[clean_data_hrqol$dementia.bin==dem])
    
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
results_weighted_bl_dem<-wghtedmod(1, clean_data_hrqol$baseline.anwgt_scaled, "log")      
results_weighted_av_dem<-wghtedmod(1, clean_data_hrqol$average.anwgt_scaled, "log")      
results_weighted_rd_dem<-wghtedmod(1, clean_data_hrqol$analytic.wgt_scaled, "log")      

results_weighted_bl_nodem<-wghtedmod(0, clean_data_hrqol$baseline.anwgt_scaled, "log")     
results_weighted_av_nodem<-wghtedmod(0, clean_data_hrqol$average.anwgt_scaled, "log")     
results_weighted_rd_nodem<-wghtedmod(0, clean_data_hrqol$analytic.wgt_scaled, "log")     

#RD models
results_weighted_bl_dem_RD<-wghtedmod(1, clean_data_hrqol$baseline.anwgt_scaled, "identity")      
results_weighted_av_dem_RD<-wghtedmod(1, clean_data_hrqol$average.anwgt_scaled, "identity")      
results_weighted_rd_dem_RD<-wghtedmod(1, clean_data_hrqol$analytic.wgt_scaled, "identity")      

results_weighted_bl_nodem_RD<-wghtedmod(0, clean_data_hrqol$baseline.anwgt_scaled, "identity")     
results_weighted_av_nodem_RD<-wghtedmod(0, clean_data_hrqol$average.anwgt_scaled, "identity")     
results_weighted_rd_nodem_RD<-wghtedmod(0, clean_data_hrqol$analytic.wgt_scaled, "identity")     




#------------------------------------------------------------------
# Format and export results #
#------------------------------------------------------------------

res_tbls<-list(results_unweighted_dem=results_unweighted_dem$outresults, 
               results_unweighted_nodem=results_unweighted_nodem$outresults,
               results_weighted_bl_dem=results_weighted_bl_dem$outresults,
               results_weighted_av_dem=results_weighted_av_dem$outresults,
               results_weighted_rd_dem=results_weighted_rd_dem$outresults,
               results_weighted_bl_nodem=results_weighted_bl_nodem$outresults,
               results_weighted_av_nodem=results_weighted_av_nodem$outresults,
               results_weighted_rd_nodem=results_weighted_rd_nodem$outresults,
               
               results_unweighted_dem_RD=results_unweighted_dem_RD$outresults, 
               results_unweighted_nodem_RD=results_unweighted_nodem_RD$outresults,
               results_weighted_bl_dem_RD=results_weighted_bl_dem_RD$outresults,
               results_weighted_av_dem_RD=results_weighted_av_dem_RD$outresults,
               results_weighted_rd_dem_RD=results_weighted_rd_dem_RD$outresults,
               results_weighted_bl_nodem_RD=results_weighted_bl_nodem_RD$outresults,
               results_weighted_av_nodem_RD=results_weighted_av_nodem_RD$outresults,
               results_weighted_rd_nodem_RD=results_weighted_rd_nodem_RD$outresults)


  for (i in 1:length(res_tbls) ){
    
  name<-names(res_tbls)[i]
  
  res_tbls[[i]]$dementia<-ifelse(grepl("nodem", substr(name, 0,100000)),0,1)
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
results_forplot$outcome2[results_forplot$outcome=="funclimits"]<-">=1 ADL limitation"
results_forplot$outcome2[results_forplot$outcome=="pain.bother"]<-"Bothered by pain"
results_forplot$outcome2[results_forplot$outcome=="poorhealth.bin"]<-"Fair/poor health"
results_forplot$outcome2[results_forplot$outcome=="prob.dep"]<-"Screen+ depresson"
results_forplot$outcome2[results_forplot$outcome=="prob.anx"]<-"Screen+ anxiety"

results_forplot$race[results_forplot$race=="black_est"] <- "Black vs. White"
results_forplot$race[results_forplot$race=="hispanic_est"] <- "Latino vs. White"
results_forplot$race[results_forplot$race=="other_est"] <- "Other vs. White"





plotresRR<-function(wt){
ggplot(data=results_forplot[(results_forplot$race=="Black vs. White" | 
                                       results_forplot$race=="Latino vs. White") & 
                                       results_forplot$measure=="RR" &
                                       results_forplot$weight==wt,])+
  geom_pointrange(aes(x=outcome2, y=value, ymin=LCI, ymax=UCI, group=as.factor(dementia), 
                      color=as.factor(dementia)), position=position_dodge(width=0.6), size=1, shape=15)+
  xlab("HRQOL indicator")+ ylab("Prevalence ratio (95% CI)")+ ylim(0.8,2.6)+facet_grid(.~race)+
  scale_color_manual(name="", labels=c("No dementia", "Dementia"), values=c("navy", "steelblue"))+
  theme_bw()+ 
  geom_hline(yintercept=1, colour="black", lwd=1) +
  theme(axis.text.x = element_text(angle = 70, hjust=1, size=12), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
  )
}

res_un_RR<-plotresRR("unweighted_")
res_bl_RR<-plotresRR("weighted_bl")
res_av_RR<-plotresRR("weighted_av")


plotresRD<-function(wt){
  ggplot(data=results_forplot[(results_forplot$race=="Black vs. White" | 
                                 results_forplot$race=="Latino vs. White") & 
                                results_forplot$measure=="RD" &
                                results_forplot$weight==wt,])+
    geom_pointrange(aes(x=outcome2, y=value, ymin=LCI, ymax=UCI, group=as.factor(dementia), 
                        color=as.factor(dementia)), position=position_dodge(width=0.6), size=1, shape=15)+
    xlab("HRQOL indicator")+ ylab("Prevalence difference (95% CI)")+ facet_grid(.~race)+
    scale_color_manual(name="", labels=c("No dementia", "Dementia"), values=c("navy", "steelblue"))+
    theme_bw()+ scale_y_continuous(labels = scales::percent, limits=c(-0.1,0.3))+
    geom_hline(yintercept=0, colour="black", lwd=1) +
    theme(axis.text.x = element_text(angle = 70, hjust=1, size=12), 
          axis.text.y = element_text(size=12), 
          axis.title.x = element_text(size=14), 
          axis.title.y = element_text(size=14), 
    )
}

res_un_RD<-plotresRD("unweighted_")
res_bl_RD<-plotresRD("weighted_bl")
res_av_RD<-plotresRD("weighted_av")


figures<-list(res_un_RR=res_un_RR, res_av_RR=res_av_RR, res_bl_RR=res_bl_RR, 
              res_un_RD=res_un_RD, res_av_RD=res_av_RD, res_bl_RD=res_bl_RD)

for (i in 1:length(figures)){
  ggsave(filename=paste0("C:/Users/ehlarson/Box/NHATS/OUTPUT/FIGURES/",names(figures)[i],".jpg"), 
         plot=eval(parse_expr(names(figures[i]))), dpi="retina")
}


save(results_all,file="C:/Users/ehlarson/Box/NHATS/OUTPUT/RR_HRQOL_pooled.Rdata")
write.xlsx(results_all, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/RR_HRQOL_pooled.xlsx")



#------------------------------------------------------------------
# Standardized predicted prevalences
#------------------------------------------------------------------

pred_prev_dem<-
pred_prev_nodem<-data.frame(outcome=outcomes, white_est=rep(NA,5), white_LCI=rep(NA,5), white_UCI=rep(NA,5), 
                            black_est=rep(NA,5), black_LCI=rep(NA,5), black_UCI=rep(NA,5), 
                            latino_est=rep(NA,5), latino_LCI=rep(NA,5), latino_UCI=rep(NA,5), 
                            other_est=rep(NA,5), other_LCI=rep(NA,5), other_UCI=rep(NA,5))


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

pred_prev_dem<-pred_prevs(results_weighted_bl_dem)
pred_prev_nodem<-pred_prevs(results_weighted_bl_nodem)
 
pred_prev_dem$dementia<-1
pred_prev_nodem$dementia<-0


pred_all<-rbind(pred_prev_dem, pred_prev_nodem)

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
pred_forplot$outcome2[pred_forplot$outcome=="funclimits"]<-">=1 ADL limitation"
pred_forplot$outcome2[pred_forplot$outcome=="pain.bother"]<-"Bothered by pain"
pred_forplot$outcome2[pred_forplot$outcome=="poorhealth.bin"]<-"Fair/poor health"
pred_forplot$outcome2[pred_forplot$outcome=="prob.dep"]<-"Screen+ depresson"
pred_forplot$outcome2[pred_forplot$outcome=="prob.anx"]<-"Screen+ anxiety"

pred_forplot$race[pred_forplot$race=="white_est"] <- "White"
pred_forplot$race[pred_forplot$race=="black_est"] <- "Black"
pred_forplot$race[pred_forplot$race=="latino_est"] <- "Latino"
pred_forplot$race[pred_forplot$race=="other_est"] <- "Other"


  ggplot(data=pred_forplot[pred_forplot$race %in% c("White", "Black", "Latino"),])+
    geom_col(aes(x=race, y=value, group=factor(dementia), 
                        fill=factor(dementia)), width=0.5, position=position_dodge(width=.5))+
    geom_errorbar(aes(x=race, ymin=LCI, ymax=UCI, group=factor(dementia)), width=0.25, position=position_dodge(width=0.5))+
    xlab("Race/ethnicity")+ ylab("Standardized predicted prevalence (95% CI)")+ facet_grid(.~outcome2)+
    scale_fill_manual(name="", labels=c("No dementia", "Dementia"), values=c("navy", "steelblue"))+
 #  scale_color_manual(name="", labels=c("No dementia", "Dementia"), values=c("navy", "steelblue"))+
    theme_bw()+ scale_y_continuous(labels = scales::percent, limits=c(0.0,1))+
    geom_hline(yintercept=0, colour="black", lwd=1) +
    theme(axis.text.x = element_text(angle = 70, hjust=1, size=12), 
          axis.text.y = element_text(size=12), 
          axis.title.x = element_text(size=14), 
          axis.title.y = element_text(size=14), 
    )

  
  ggplot(data=pred_forplot[pred_forplot$race %in% c("White", "Black", "Latino"),])+
    geom_col(aes(x=factor(dementia), y=value, group=factor(race), 
                 fill=factor(race)), width=0.75, position=position_dodge(width=.75))+
    geom_errorbar(aes(x=factor(dementia), ymin=LCI, ymax=UCI, group=factor(race)), width=0.5, position=position_dodge(width=0.75))+
    xlab("Race/ethnicity")+ ylab("Standardized predicted prevalence (95% CI)")+ facet_grid(.~outcome2)+
#    scale_fill_manual(name="", labels=c("No dementia", "Dementia"), values=c("navy", "steelblue"))+
    scale_fill_manual(name="", labels=c("Black", "Latino","White"), values=c("purple", "orange", "forestgreen"))+
    theme_bw()+ scale_y_continuous(labels = scales::percent, limits=c(0.0,1))+scale_x_discrete(labels=c("No dementia", "Dementia"))+
    geom_hline(yintercept=0, colour="black", lwd=1) +
    theme(axis.text.x = element_text(angle = 70, hjust=1, size=12), 
          axis.text.y = element_text(size=12), 
          axis.title.x = element_text(size=14), 
          axis.title.y = element_text(size=14), 
    )
  

