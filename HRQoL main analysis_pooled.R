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


summary(model)

summary(model)$coefficients[2,"Estimate"]
      
test<-as.double(coef(model))
test[2]

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
results_weighted_avg_dem<-wghtedmod(1, clean_data_hrqol$average.anwgt)      
results_weighted_rd_dem<-wghtedmod(1, clean_data_hrqol$analytic.wgt)      

results_weighted_bl_nodem<-wghtedmod(0, clean_data_hrqol$baseline.anwgt)     
results_weighted_avg_nodem<-wghtedmod(0, clean_data_hrqol$average.anwgt)     
results_weighted_rd_nodem<-wghtedmod(0, clean_data_hrqol$analytic.wgt)     



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
  geom_pointrange(aes(x=outcome2, y=RR, ymin=lci, ymax=uci, group=as.factor(dementia), color=as.factor(dementia)), position=position_dodge(width=0.6), size=1, shape=15)+
  xlab("HRQOL indicator")+ ylab("Prevalence ratio (95% CI)")+ facet_grid(.~race)+
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
