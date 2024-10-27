###Load resultsinto R session
setwd("/Users/neetikulkarni/Desktop/Summer Efficiency Frontier Research")
source("Com_model_code.R")

###############################################################################
#Plot all results on common figures
###############################################################################

##Add in intervention to each dataframe 
res$intervention = "SQ"
res1$intervention = "SQ"
res2$intervention = "Wegovy"
res3$intervention = "Wegovy"
res4$intervention = "Surgery"
res5$intervention = "Surgery"

##Add in race to each dataframe
res$race = "White"
res1$race = "Black"
res2$race = "White"
res3$race = "Black"
res4$race = "White"
res5$race = "Black"

##Merge dataframes

#make data have consistent columns
res2 = subset(res2, select = -c(1:2))
res3 = subset(res3, select = -c(1:2))
res4 = subset(res4, select = -c(1:2))
res5 = subset(res5, select = -c(1:2))

res$obesity_class_I = 0
res1$obesity_class_I = 0

res[, 1:4] = res[, 1:4] / 102874
res2[, 1:5] = res2[, 1:5] / 102874
res4[, 1:5] = res4[, 1:5] / 102874

res1[, 1:4] = res1[, 1:4] / 16648
res3[, 1:5] = res3[, 1:5] / 16648
res5[, 1:5] = res5[, 1:5] / 16648

#merge data by race
White_data = rbind(res,res2,res4)
Black_data = rbind(res1,res3,res5)

#merge data by intervention
SQ_data = rbind(res,res1)
Wegovy_data = rbind(res2,res3)
Surgery_data = rbind(res4,res5)

###########Plot by race########################################################

#White population
colors <- c("Obesity Class I"="#db7093","Obesity Class II" = "#40e0d0", 
            "Obesity Class III.I" = "#daa520", 
            "Obesity Class III.II" = "darkgreen",
            "Death" = "purple")

ggplot(data=White_data, aes(x=time, linetype=intervention)) + 
  geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
  geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
  geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
  geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
  geom_line(aes(y=death, color="Death")) +  
  ggtitle("Comparison of 3 Interventions for White Population") +
  labs(x = "Months",
       y = "Pop Count",
       color = "State",
       linetype = "Intervention") +
  scale_color_manual(values = colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Black population

ggplot(data=Black_data, aes(x=time, linetype=intervention)) + 
  geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
  geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
  geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
  geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
  geom_line(aes(y=death, color="Death")) +  
  ggtitle("Comparison of 3 Interventions for Black Population") +
  labs(x = "Months",
       y = "Pop Count",
       color = "State",
       linetype = "Intervention") +
  scale_color_manual(values = colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#####Plot by intervention#######################################################

colors <- c("Obesity Class I"="#db7093","Obesity Class II" = "#40e0d0", 
            "Obesity Class III.I" = "#daa520", 
            "Obesity Class III.II" = "darkgreen",
            "Death" = "purple")

ggplot(data=SQ_data, aes(x=time, linetype=race)) + 
  geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
  geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
  geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
  geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
  geom_line(aes(y=death, color="Death")) +  
  ggtitle("Status Quo Intervention for White and Black Population") +
  labs(x = "Months",
       y = "Population Proportion",
       color = "State",
       linetype = "Race") +
  scale_color_manual(values = colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=Wegovy_data, aes(x=time, linetype=race)) + 
  geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
  geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
  geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
  geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
  geom_line(aes(y=death, color="Death")) +  
  ggtitle("Wegovy Intervention for White and Black Population") +
  labs(x = "Months",
       y = "Population Proportion",
       color = "State",
       linetype = "Race") +
  scale_color_manual(values = colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=Surgery_data, aes(x=time, linetype=race)) + 
  geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
  geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
  geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
  geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
  geom_line(aes(y=death, color="Death")) +  
  ggtitle("Bariatric Surgery Intervention for White and Black Population") +
  labs(x = "Months",
       y = "Population Proportion",
       color = "State",
       linetype = "Race") +
  scale_color_manual(values = colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#####Cost and QALY plot#########################################################

##White population

##Status quo
QALY_SQ_list = as.data.frame(QALY_SQ_list)
Cum_SQ_QALYs = as.data.frame(t(QALY_SQ_list), row.names = FALSE)
Cum_SQ_QALYs$time = seq(1:60)
names(Cum_SQ_QALYs) = c('Cum_qalys','Months')

##Wegovy
Cost_W_list2 = as.data.frame(Cost_W_list2)
Cum_Wegovy_costs = as.data.frame(t(Cost_W_list2), row.names = FALSE)
Cum_Wegovy_costs$time = seq(1:60)
names(Cum_Wegovy_costs) = c('Cum_costs','Months')

QALY_W_list2 = as.data.frame(QALY_W_list2)
Cum_Wegovy_QALYs = as.data.frame(t(QALY_W_list2), row.names = FALSE)
Cum_Wegovy_QALYs$time = seq(1:60)
names(Cum_Wegovy_QALYs) = c('Cum_qalys','Months')

##Surgery 
QALY_S_list4 = as.data.frame(QALY_S_list4)
Cum_Surgery_QALYs = as.data.frame(t(QALY_S_list4), row.names = FALSE)
Cum_Surgery_QALYs$time = seq(1:60)
names(Cum_Surgery_QALYs) = c('Cum_qalys','Months')

##Cost plot

ggplot(data=Cum_Wegovy_costs, aes(x=Months, y=Cum_costs)) + 
  geom_line(aes(color="Wegovy"))  +  
  #geom_hline(yintercept=1832678380.08, color="#6495ed", linetype="solid") +
  geom_line(aes(y=totalCosts4, color="Bariatric Surgery"), linetype="solid") +
  geom_line(aes(y=0, color="Status Quo"), linetype="dashed") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Cumulative Costs") + ggtitle("Cumulative Costs of 3 Interventions for White Population") +
  labs(color="Intervention") + 
  scale_color_manual(values = c("Status Quo" = "#8b0000", 
                                "Wegovy" = "black",
                                "Bariatric Surgery" = "#6495ed")) 

##QALY plot

ggplot(data=Cum_Wegovy_QALYs, aes(x=Months, y=Cum_qalys)) + 
  geom_line(aes(color="Wegovy")) +  
  #geom_hline(yintercept=1832678380.08, color="#6495ed", linetype="solid") +
  geom_line(data=Cum_Surgery_QALYs,aes(y=Cum_qalys, color="Bariatric Surgery"), linetype="solid") +
  geom_line(data=Cum_SQ_QALYs,aes(y=Cum_qalys, color="Status Quo"), linetype="solid") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Cumulative QALYs") + ggtitle("Cumulative QALYs of 3 Interventions for White Population") +
  labs(color="Intervention") + 
  scale_color_manual(values = c("Status Quo" = "#8b0000", 
                                "Wegovy" = "black",
                                "Bariatric Surgery" = "#6495ed")) 

##Black population

##Status quo
QALY_SQ_list1 = as.data.frame(QALY_SQ_list1)
Cum_SQ_QALYs_Black = as.data.frame(t(QALY_SQ_list1), row.names = FALSE)
Cum_SQ_QALYs_Black$time = seq(1:60)
names(Cum_SQ_QALYs_Black) = c('Cum_qalys','Months')

##Wegovy
Cost_W_list3 = as.data.frame(Cost_W_list3)
Cum_Wegovy_costs_Black = as.data.frame(t(Cost_W_list3), row.names = FALSE)
Cum_Wegovy_costs_Black$time = seq(1:60)
names(Cum_Wegovy_costs_Black) = c('Cum_costs','Months')

QALY_W_list3 = as.data.frame(QALY_W_list3)
Cum_Wegovy_QALYs_Black = as.data.frame(t(QALY_W_list3), row.names = FALSE)
Cum_Wegovy_QALYs_Black$time = seq(1:60)
names(Cum_Wegovy_QALYs_Black) = c('Cum_qalys','Months')

##Surgery 
QALY_S_list5 = as.data.frame(QALY_S_list5)
Cum_Surgery_QALYs_Black = as.data.frame(t(QALY_S_list5), row.names = FALSE)
Cum_Surgery_QALYs_Black$time = seq(1:60)
names(Cum_Surgery_QALYs_Black) = c('Cum_qalys','Months')


##Cost plot
ggplot(data=Cum_Wegovy_costs_Black, aes(x=Months, y=Cum_costs)) + 
  geom_line(aes(color="Wegovy")) + geom_point(aes(color="Wegovy")) +  
  #geom_hline(yintercept=1832678380.08, color="#6495ed", linetype="solid") +
  geom_line(aes(y=totalCosts5, color="Bariatric Surgery"), linetype="solid") +
  geom_line(aes(y=0, color="Status Quo"), linetype="dashed") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Cumulative Costs") + ggtitle("Cumulative Costs of 3 Interventions for Black Population") +
  labs(color="Intervention") + 
  scale_color_manual(values = c("Status Quo" = "#8b0000", 
                                "Wegovy" = "black",
                                "Bariatric Surgery" = "#6495ed")) 

##QALY plot

ggplot(data=Cum_Wegovy_QALYs_Black, aes(x=Months, y=Cum_qalys)) + 
  geom_line(aes(color="Wegovy")) +  
  #geom_hline(yintercept=1832678380.08, color="#6495ed", linetype="solid") +
  geom_line(data=Cum_Surgery_QALYs_Black,aes(y=Cum_qalys, color="Bariatric Surgery"), linetype="solid") +
  geom_line(data=Cum_SQ_QALYs_Black,aes(y=Cum_qalys, color="Status Quo"), linetype="solid") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Cumulative QALYs") + ggtitle("Cumulative QALYs of 3 Interventions for Black Population") +
  labs(color="Intervention") + 
  scale_color_manual(values = c("Status Quo" = "#8b0000", 
                                "Wegovy" = "black",
                                "Bariatric Surgery" = "#6495ed")) 