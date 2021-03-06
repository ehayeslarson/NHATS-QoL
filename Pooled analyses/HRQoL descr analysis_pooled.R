#------------------------------------------------------------------
# Title: HRQoL descriptive analysis pooled
# Author: Eleanor Hayes-Larson
#------------------------------------------------------------------

#------------------------------------------------------------------
# Loading packages, options and initializations #
#------------------------------------------------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "ggplot2", "gee", "wgeesel", 
       "survey", "tableone", "openxlsx", "rlang")

#------------------------------------------------------------------
# Load clean data#
#------------------------------------------------------------------
load("C:/Users/ehlarson/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")

clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

#Keep baseline observation for participants in clean_data_hrqol
idlist<-clean_data_hrqol %>% distinct(spid)
idlist<-idlist[,1]
clean_data_hrqol_baseline<-clean_data %>% filter(.,first.obs==1, spid %in% idlist)

catvars<-c("age.cat", "female","edu.7cat", "resid.care", "cens.area", "born.us", 
           "proxy", "proxy.fam", "sr.highbp","sr.diabetes","sr.stroke", "sr.cancer", "dementia.status", "round")
catvar_names<-c("Age (years)", "Female", "Education attained", 
                "Lives in residential care setting", "Census region", "Born in US", 
                "Proxy answered survey", "Familiarity of proxy with routine", 
                "Self-reported high blood pressure","Self-reported diabetes",
                "Self-reported stroke", "Self-reported cancer", "Dementia", "Enrolled in Round")


#------------------------------------------------------------------
# ---Unweighted analyses--- #
#------------------------------------------------------------------
  
    
    T1results_unweighted<-matrix(nrow=1, ncol=4) 
    T1results_unweighted[1,]<- c("Race/ethnicity total",table(clean_data_hrqol_baseline$race.eth))
  

    for (i in 1:length(catvars)){
    tab.to.add<-table(eval(parse_expr(paste0("clean_data_hrqol_baseline$",catvars[i])))
                      ,clean_data_hrqol_baseline$race.eth, exclude=NULL)
    labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
    T1results_unweighted<-rbind(T1results_unweighted, c(paste(catvar_names[i]),rep(NA,3))) 
    T1results_unweighted<-rbind(T1results_unweighted,cbind(labs, tab.to.add))
    }

    colnames(T1results_unweighted)<-c("Variable name", "Non-Latino white", "Black", "Latino") 

    rownames(T1results_unweighted)<-NULL
    T1results_unweighted[is.na(T1results_unweighted[,1]),"Variable name"]<-"Missing"
    
    
    
#------------------------------------------------------------------
# ---Weighted analyses--- #
#------------------------------------------------------------------
  nhats_design<-svydesign(data=clean_data_hrqol_baseline, id=~cluster, strata=~stratum, weights=~analytic.wgt, nest=T)
  
  svytable(~edu.cat+race.eth, nhats_design, exclude=NULL, round=T, na.action=na.pass, addNA=TRUE)
  
  T1results_weighted<-matrix(nrow=1, ncol=4) 
  T1results_weighted[1,]<- c("Race/ethnicity total",
                             svytable(~race.eth, nhats_design, exclude=NULL, round=T, 
                                      na.action=na.pass, addNA=TRUE))
  
  for (i in 1:length(catvars)){
    tab.to.add<-svytable(~eval(parse_expr(paste0("clean_data_hrqol_baseline$",catvars[i])))+race.eth, 
                         nhats_design, exclude=NULL, round=T, na.action=na.pass, addNA=TRUE)
    labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
    T1results_weighted<-rbind(T1results_weighted, c(paste(catvar_names[i]),rep(NA,3))) 
    T1results_weighted<-rbind(T1results_weighted,cbind(labs, tab.to.add))
  }
  
  colnames(T1results_weighted)<-c("Variable name", "Non-Latino white", "Black", "Latino") 
  rownames(T1results_weighted)<-NULL
  T1results_weighted[is.na(T1results_weighted[,1]),"Variable name"]<-"Missing"
  
#------------------------------------------------------------------
# Save tables #
#------------------------------------------------------------------
  T1.list <- list("Unweighted" = T1results_unweighted, "Weighted" = T1results_weighted)
  write.xlsx(T1.list, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/Table1.xlsx")
  
  