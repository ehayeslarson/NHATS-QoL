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
                               black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                               black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                               black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))

          for (i in 1:length(outcomes)){
                        outcome<-outcomes[i]
            
            model<-geeglm(as.formula(paste(outcome,"~as.factor(race.eth)+factor(age.cat, ordered=F)+female")), 
                       id=spid, data=indata, corstr="exchangeable", family=poisson(link=log))
            
            print(summary(model))
            
            for (j in 2:4){
              outresults[i,j] <- exp(coef(model)[j])
              outresults[i,j+3] <- exp(coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"])
              outresults[i,j+6] <- exp(coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"])
            }
          }
        return(outresults=outresults)
      }
      

      results_unweighted_dem<-unwghtedmod(clean_data_hrqol_dem)      
      results_unweighted_nodem<-unwghtedmod(clean_data_hrqol_nodem)      
  
      
      
      
      unwghtedmodRD<-function(indata){
        outresults<-data.frame(outcome=outcomes,
                               black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                               black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                               black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
        
        for (i in 1:length(outcomes)){
          outcome<-outcomes[i]
          
          model<-geeglm(as.formula(paste(outcome,"~as.factor(race.eth)+factor(age.cat, ordered=F)+female")), 
                        id=spid, data=indata, corstr="exchangeable", family=poisson(link=identity))
          print(summary(model))
          
          for (j in 2:4){
            outresults[i,j] <- coef(model)[j]
            outresults[i,j+3] <- coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"]
            outresults[i,j+6] <- coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"]
          }
        }
        return(outresults)
      }
      
      
      results_unweighted_dem_RD<-unwghtedmodRD(clean_data_hrqol_dem)      
      results_unweighted_nodem_RD<-unwghtedmodRD(clean_data_hrqol_nodem)      
      
      
# ---Weighted analyses--- #



wghtedmod<-function(dem,weightvar){
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste(outcome,"~as.factor(race.eth)+factor(age.cat, ordered=F)+female")), 
               id=spid, data=clean_data_hrqol[clean_data_hrqol$dementia.bin==dem,], 
               corstr="exchangeable", family=poisson(link=log), weights=weightvar[clean_data_hrqol$dementia.bin==dem])
    print(summary(model))
    
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


wghtedmodRD<-function(dem,weightvar){
  outresults<-data.frame(outcome=outcomes,
                         black_est=rep(NA,5), hispanic_est=rep(NA,5), other_est=rep(NA,5), 
                         black_lci=rep(NA,5), hispanic_lci=rep(NA,5),  other_lci=rep(NA,5),
                         black_uci=rep(NA,5),  hispanic_uci=rep(NA,5), other_uci=rep(NA,5))
  
  for (i in 1:length(outcomes)){
    outcome<-outcomes[i]
    
    model<-geeglm(as.formula(paste(outcome,"~as.factor(race.eth)+as.factor(age.cat)+female")), 
                  id=spid, data=clean_data_hrqol[clean_data_hrqol$dementia.bin==dem,], 
                  corstr="exchangeable", family=poisson(link=identity), weights=weightvar[clean_data_hrqol$dementia.bin==dem])
    print(summary(model))
    
    for (j in 2:4){
      outresults[i,j] <- coef(model)[j]
      outresults[i,j+3] <- coef(model)[j]-1.96*summary(model)$coefficients[j,"Std.err"]
      outresults[i,j+6] <- coef(model)[j]+1.96*summary(model)$coefficients[j,"Std.err"]
    }
  }
  return(outresults)
}


results_weighted_bl_dem_RD<-wghtedmodRD(1, clean_data_hrqol$baseline.anwgt)      
results_weighted_av_dem_RD<-wghtedmodRD(1, clean_data_hrqol$average.anwgt)      
results_weighted_rd_dem_RD<-wghtedmodRD(1, clean_data_hrqol$analytic.wgt)      

results_weighted_bl_nodem_RD<-wghtedmodRD(0, clean_data_hrqol$baseline.anwgt)     
results_weighted_av_nodem_RD<-wghtedmodRD(0, clean_data_hrqol$average.anwgt)     
results_weighted_rd_nodem_RD<-wghtedmodRD(0, clean_data_hrqol$analytic.wgt)     




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
               results_weighted_rd_nodem=results_weighted_rd_nodem,
               
               results_unweighted_dem_RD=results_unweighted_dem_RD, 
               results_unweighted_nodem_RD=results_unweighted_nodem_RD,
               results_weighted_bl_dem_RD=results_weighted_bl_dem_RD,
               results_weighted_av_dem_RD=results_weighted_av_dem_RD,
               results_weighted_rd_dem_RD=results_weighted_rd_dem_RD,
               results_weighted_bl_nodem_RD=results_weighted_bl_nodem_RD,
               results_weighted_av_nodem_RD=results_weighted_av_nodem_RD,
               results_weighted_rd_nodem_RD=results_weighted_rd_nodem_RD)


  for (i in 1:length(res_tbls) ){
  
  name<-names(res_tbls)[i]
  
  res_tbls[[i]][,"dementia"]<-ifelse(grepl("nodem", substr(name, 0,100000)),0,1)
  res_tbls[[i]][,"weight"]<-paste(substr(name, 9, 19))
  res_tbls[[i]][,"measure"]<-ifelse(grepl("RD", substr(name, 0,100000)),"RD","RR")
  
  }
  

results_all<-do.call(rbind,res_tbls)

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

res_un_RR<-plotres("unweighted_")
res_bl_RR<-plotres("weighted_bl")
res_av_RR<-plotres("weighted_av")


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


save(results_all,file="C:/Users/ehlarson/Box/NHATS/OUTPUT/RR_HRQOL_pooled.Rdata")
write.xlsx(results_all, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/RR_HRQOL_pooled.xlsx")
