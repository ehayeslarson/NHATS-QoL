load("C:/Users/ehlarson/Box/NHATS/OUTPUT/HRQOL_pooled.Rdata")
res_main<-results_all
load("C:/Users/ehlarson/Box/NHATS/OUTPUT/HRQOL_pooled_sens.Rdata")
res_sens<-results_all



results_forplot<-pivot_longer(res_main, cols = c("black_est", "hispanic_est", "other_est"),
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
    xlab("HRQOL indicator")+ ylab("Prevalence ratio (95% CI)")+ ylim(0.7,2.6)+facet_grid(.~race)+
    scale_color_manual(name="", labels=c("Probable dementia", "Possible dementia", "No dementia"), values=c("navy", "steelblue", "lightblue"))+
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

res_bl_RR<-plotresRR("weighted_bl")




results_forplot<-pivot_longer(res_sens, cols = c("black_est", "hispanic_est", "other_est"),
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
    xlab("HRQOL indicator")+ ylab("Prevalence ratio (95% CI)")+ ylim(0.7,2.7)+facet_grid(.~race)+
    scale_color_manual(name="", labels=c("Probable dementia", "Possible dementia", "No dementia"), values=c("tan4", "tan3", "tan1"))+
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

res_bl_RR_sens<-plotresRR("weighted_bl")

res_bl_RR + geom_pointrange(data=results_forplot[(results_forplot$race=="Black vs. White" | 
                                                    results_forplot$race=="Latino vs. White") & 
                                                   results_forplot$measure=="RR" &
                                                   results_forplot$weight=="weighted_bl",], aes(x=outcome2, y=value, ymin=LCI, ymax=UCI, group=as.factor(dementia), 
                                fill=factor(dementia)), position=position_dodge(width=0.6), size=1, shape=22)+
  ylim(0.7,2.7)+facet_grid(.~race)+
  scale_fill_manual(name="", labels=c("Probable dementia", "Possible dementia", "No dementia"), values=c("tan4", "tan3", "tan1"))+
  theme_bw()+ 
  guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
  geom_hline(yintercept=1, colour="black", lwd=1) +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
        legend.position = "bottom"
  )+ coord_flip()
)






main<-res_bl_RR$data
main$analysis<-"Primary"


sens<-res_bl_RR_sens$data
sens$analysis<-"Sensitivity"

all<-rbind(main,sens)

all$colorvar<-paste0(all$dementia,all$analysis)


ggplot(data=all)+
  geom_pointrange(aes(x=outcome2, y=value, ymin=LCI, ymax=UCI, group=as.factor(dementia), 
                      color=as.factor(colorvar)), position=position_dodge(width=0.5), size=1, shape=15, alpha=0.75)+
  xlab("HRQOL indicator")+ ylab("Prevalence ratio (95% CI)")+ ylim(0.6,2.7)+facet_grid(.~race)+
  scale_color_manual(name="", labels=c("Main Analysis, Probable dementia", 
                                       "Sensitivity Analysis, Probable dementia", 
                                       "Main Analysis, Possible dementia", 
                                       "Sensitivity Analysis, Possible dementia", 
                                       "Main Analysis, No dementia", 
                                       "Sensitivity Analysis, No dementia"), 
                                        values=c("navy","tan4", "steelblue", "tan3","lightblue",  "tan1"))+
  theme_bw()+ 
  guides(color = guide_legend(override.aes = list(linetype = 0, size=1)))+
  geom_hline(yintercept=1, colour="black", lwd=1) +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14), 
        legend.position = "bottom"
  )+ coord_flip()



test<-clean_data[clean_data$spid==10000175,]


