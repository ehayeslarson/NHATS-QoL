#------------------------------------------------------------------
# Title: HRQoL descriptive analysis pooled proxy vs. no proxy
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


#code checking TMM
load("C:/Users/tmobley/Box/NHATS/DATA/analysis_datasets/QOL_DEM_analysis_clean_pooled.RData")


clean_data_hrqol<-clean_data[clean_data$comp.case.HRQoL==1,]

#Keep baseline observation for participants in clean_data_hrqol
idlist<-clean_data_hrqol %>% distinct(spid)
idlist<-idlist[,1]
clean_data_hrqol_baseline<-clean_data %>% filter(.,first.obs==1, spid %in% idlist)

catvars<-c("race.eth", "age.cat", "female", "resid.care", 
            "sr.highbp","sr.diabetes","sr.stroke", "sr.cancer", "dementia.status")
catvar_names<-c("Race/ethnicity", "Age (years)", "Female", 
                "Lives in residential care setting", 
                "Self-reported high blood pressure","Self-reported diabetes",
                "Self-reported stroke", "Self-reported cancer", "Dementia")


#------------------------------------------------------------------
# ---Unweighted analyses--- #
#------------------------------------------------------------------

#Total
T1results_unweighted<-matrix(nrow=1, ncol=3)
T1results_unweighted[1,]<- c("Proxy total",table(clean_data_hrqol$proxy))


#code checking
T1results_unweighted
#end


for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("clean_data_hrqol$",catvars[i])))
                    ,clean_data_hrqol$proxy, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_unweighted<-rbind(T1results_unweighted, c(paste(catvar_names[i]),rep(NA,2))) 
  T1results_unweighted<-rbind(T1results_unweighted,cbind(labs, tab.to.add))
}

colnames(T1results_unweighted)<-c("Variable name", "No proxy", "Proxy")

rownames(T1results_unweighted)<-NULL
T1results_unweighted[is.na(T1results_unweighted[,1]),"Variable name"]<-"Missing"


#code checking
T1results_unweighted
#end


#Probable dementia

clean_data_hrqol_probdem<-clean_data_hrqol[clean_data_hrqol$dementia.status==1,]


#code checking
table(clean_data_hrqol_probdem$dementia.status, exclude=NULL)
#end


T1results_unweighted_probdem<-matrix(nrow=1, ncol=3)
T1results_unweighted_probdem[1,]<- c("Proxy total",table(clean_data_hrqol_probdem$proxy))


for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("clean_data_hrqol_probdem$",catvars[i])))
                    ,clean_data_hrqol_probdem$proxy, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_unweighted_probdem<-rbind(T1results_unweighted_probdem, c(paste(catvar_names[i]),rep(NA,2))) 
  T1results_unweighted_probdem<-rbind(T1results_unweighted_probdem,cbind(labs, tab.to.add))
}

colnames(T1results_unweighted_probdem)<-c("Variable name", "No proxy", "Proxy")

rownames(T1results_unweighted_probdem)<-NULL
T1results_unweighted_probdem[is.na(T1results_unweighted_probdem[,1]),"Variable name"]<-"Missing"



#------------------------------------------------------------------
# ---Weighted analyses--- #
#------------------------------------------------------------------

#Total
nhats_design<-svydesign(data=clean_data_hrqol, id=~cluster, strata=~stratum, weights=~analytic.wgt, nest=T)


T1results_weighted<-matrix(nrow=1, ncol=3) 
T1results_weighted[1,]<- c("Proxy total",
                           svytable(~proxy, nhats_design, exclude=NULL, round=T, 
                                    na.action=na.pass, addNA=TRUE))

for (i in 1:length(catvars)){
  tab.to.add<-svytable(~eval(parse_expr(paste0("clean_data_hrqol$",catvars[i])))+proxy, 
                       nhats_design, exclude=NULL, round=T, na.action=na.pass, addNA=TRUE)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_weighted<-rbind(T1results_weighted, c(paste(catvar_names[i]),rep(NA,2)))
  T1results_weighted<-rbind(T1results_weighted,cbind(labs, tab.to.add))
}

colnames(T1results_weighted)<-c("Variable name", "No proxy", "Proxy")
rownames(T1results_weighted)<-NULL
T1results_weighted[is.na(T1results_weighted[,1]),"Variable name"]<-"Missing"

#Probdem
nhats_design_probdem<-svydesign(data=clean_data_hrqol_probdem, id=~cluster, strata=~stratum, weights=~analytic.wgt, nest=T)


T1results_weighted_probdem<-matrix(nrow=1, ncol=3)
T1results_weighted_probdem[1,]<- c("Proxy total",
                           svytable(~proxy, nhats_design_probdem, exclude=NULL, round=T, 
                                    na.action=na.pass, addNA=TRUE))

for (i in 1:length(catvars)){
  tab.to.add<-svytable(~eval(parse_expr(paste0("clean_data_hrqol_probdem$",catvars[i])))+proxy, 
                       nhats_design_probdem, exclude=NULL, round=T, na.action=na.pass, addNA=TRUE)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_weighted_probdem<-rbind(T1results_weighted_probdem, c(paste(catvar_names[i]),rep(NA,2)))
  T1results_weighted_probdem<-rbind(T1results_weighted_probdem,cbind(labs, tab.to.add))
}

colnames(T1results_weighted_probdem)<-c("Variable name", "No proxy", "Proxy")
rownames(T1results_weighted_probdem)<-NULL
T1results_weighted_probdem[is.na(T1results_weighted_probdem[,1]),"Variable name"]<-"Missing"



#------------------------------------------------------------------
# Save tables #
#------------------------------------------------------------------
T1.list <- list("Unweighted_total" = T1results_unweighted, "Unweighted_probdem" = T1results_unweighted_probdem, 
                "Weighted_total" = T1results_weighted, "Weighted_probdem" = T1results_weighted_probdem)
write.xlsx(T1.list, file = "C:/Users/ehlarson/Box/NHATS/OUTPUT/Table1proxy.xlsx")


#code checking
write.xlsx(T1.list, file = "C:/Users/tmobley/Desktop/Git_Repos/NHATS-QoL/Output/Table1proxy.xlsx")

