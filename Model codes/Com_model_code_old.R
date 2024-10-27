###############################################################################
#Status quo model--White Population
###############################################################################
library(ggplot2)

markovProbs = c(0.99142,0,0,0,
                0.0058,0.9939,0,0,
                0,0.00254,0.99543,0,
                0.00278,0.00356,0.00457,1.0)
M = matrix(markovProbs,nrow=4,ncol=4,byrow=TRUE)
print(M)

utilityValues = c(0.76, 0.71, 0.66, 0.0) 

states=c(1,2,3,4)
nMonths = 60
stateTraj = matrix(0,nrow=nMonths,ncol=4)
initStateVec = c(52055,25409,25409,0)
currStateVec = initStateVec

#Initalize QALY vector
totalQALYs = 0
QALY_SQ_list = list()

for (t in 1:nMonths) {
  stateTraj[t,] = currStateVec
  
  #Calculate this months QALYs
  currQALYs = sum(currStateVec*utilityValues)
  totalQALYs = totalQALYs + currQALYs
  QALY_SQ_list[t] = totalQALYs
  #draw next state
  #transitionProbs = M[,currState]
  #nextState = sample(states,1,prob=transitionProbs)
  #print(nextState)
  nextStateVec = M %*% currStateVec
  #update current state
  currStateVec = nextStateVec
}
print(stateTraj)

res = as.data.frame(stateTraj)
names(res) = c('obesity_class_II','obesity_class_III_I','obesity_class_III_II', 'death')
res$time = seq(1:60)

# Plot the results!

# colors <- c("Obesity Class II" = "blue", 
#             "Obesity Class III.I" = "red", 
#             "Obesity Class III.II" = "orange",
#             "Death" = "purple")
# 
# ggplot(data=res, aes(x=time)) + geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
#   geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
#   geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
#   geom_line(aes(y=death, color="Death")) +
#   ggtitle("Status Quo: 5 Year Change in White Population") +
#   labs(x = "Months",
#        y = "Pop Count",
#        color = "State") +
#   scale_color_manual(values = colors) + 
#   scale_y_continuous(breaks=c(0,20000,40000,60000),limits=c(0, 60000)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

###############################################################################
#Status quo model--Black Population
###############################################################################

markovProbs = c(0.99015,0,0,0,
                0.0058,0.99227,0,0,
                0,0.00254,0.99333,0,
                0.00405,0.00519,0.00667,1.0)
M = matrix(markovProbs,nrow=4,ncol=4,byrow=TRUE)
print(M)

utilityValues = c(0.76, 0.71, 0.66, 0.0) 

states=c(1,2,3,4)
nMonths = 60
stateTraj = matrix(0,nrow=nMonths,ncol=4)
initStateVec = c(8318,4165,4165,0)
currStateVec = initStateVec

#Initalize QALY vector
totalQALYs1 = 0
QALY_SQ_list1 = list()

for (t in 1:nMonths) {
  stateTraj[t,] = currStateVec
  
  #Calculate this months QALYs
  currQALYs1 = sum(currStateVec*utilityValues)
  totalQALYs1 = totalQALYs1 + currQALYs1
  QALY_SQ_list1[t] = totalQALYs1
  #draw next state
  #transitionProbs = M[,currState]
  #nextState = sample(states,1,prob=transitionProbs)
  #print(nextState)
  nextStateVec = M %*% currStateVec
  #update current state
  currStateVec = nextStateVec
}
print(stateTraj)

res1 = as.data.frame(stateTraj)
names(res1) = c('obesity_class_II','obesity_class_III_I','obesity_class_III_II', 'death')
res1$time = seq(1:60)

# Plot the results!

# colors <- c("Obesity Class II" = "blue", 
#             "Obesity Class III.I" = "red", 
#             "Obesity Class III.II" = "orange",
#             "Death" = "purple")
# 
# ggplot(data=res1, aes(x=time)) + geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
#   geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
#   geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
#   geom_line(aes(y=death, color="Death")) +
#   ggtitle("Status Quo: 5 Year Change in Black Population") +
#   labs(x = "Months",
#        y = "Pop Count",
#        color = "State") +
#   scale_color_manual(values = colors) +
#   scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000),limits=c(0, 10000)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

###############################################################################
#Wegovy model--White Population
###############################################################################

markovProbs1 = c(0.99867,0,0,0,0,0,0,
                0,0.99855,0,0,0,0,0,
                0,0,0.99821,0.136,0,0,0,
                0,0,0,0.86164,0.136,0,0,
                0,0,0,0,0.86097,0.068,0,
                0,0,0,0,0,0.92811,0,
                0.00133,0.00145,0.00179,0.00236,0.00303,0.00389,1.0)

markovProbs2 = c(0.99843,0,0,0,0,0,0,
                 0.00024,0.99831,0,0,0,0,0,
                 0,0.00024,0.99797,0,0,0,0,
                 0,0,0.00024,0.9974,0,0,0,
                 0,0,0,0.00024,0.99685,0,0,
                 0,0,0,0,0.00012,0.99611,0,
                 0.00133,0.00145,0.00179,0.00236,0.00303,0.00389,1.0)
                
                
M1 = matrix(markovProbs1,nrow=7,ncol=7,byrow=TRUE)
M2 = matrix(markovProbs2,nrow=7,ncol=7,byrow=TRUE)

utilityValues = c(0.81,0.81,0.79,0.76, 0.71, 0.66, 0.0) 
costValue = 1349.02

states=c(1,2,3,4,5,6,7)
nMonths1 = 12
nMonths2 = 48
totalMonths = nMonths1 + nMonths2


stateTraj = matrix(0,nrow=totalMonths,ncol=7)
initStateVec = c(0,0,0,52055,25409,25409,0)
currStateVec = initStateVec

#Initalize QALY vector and cost vector
totalQALYs2 = 0
totalCosts2 = 0
QALY_W_list2 = list()
Cost_W_list2 = list()

for (t in 1:nMonths) {
  stateTraj[t,] = currStateVec
  
  #Calculate this months QALYs
  currQALYs2 = sum(currStateVec*utilityValues)
  totalQALYs2 = totalQALYs2 + currQALYs2
  QALY_W_list2[t] = totalQALYs2
  
  #Calculate this months costs
  currCosts2 = sum(currStateVec*costValue)
  totalCosts2 = totalCosts2 + currCosts2
  Cost_W_list2[t] = totalCosts2
  
  if (t <= nMonths1){
    nextStateVec = M1 %*% currStateVec
  } else {
    nextStateVec = M2 %*% currStateVec
  }
  #draw next state
  #transitionProbs = M[,currState]
  #nextState = sample(states,1,prob=transitionProbs)
  #print(nextState)
  
  #update current state
  currStateVec = nextStateVec
}
print(stateTraj)

res2 = as.data.frame(stateTraj)
names(res2) = c('normal_weight',
               'overweight',
               'obesity_class_I',
               'obesity_class_II',
               'obesity_class_III_I',
               'obesity_class_III_II',
               'death')
res2$time = seq(1:totalMonths)

# Plot the results!

# colors <- c("Normal Weight"="#db7093","Overweight"="#40e0d0",
#   "Obesity Class I"="#eee8aa","Obesity Class II" = "blue", 
#             "Obesity Class III.I" = "red", 
#             "Obesity Class III.II" = "orange",
#             "Death" = "purple")
# 
# ggplot(data=res2, aes(x=time)) + 
#   #geom_line(aes(y=normal_weight, color="Normal Weight")) +
#   #geom_line(aes(y=overweight, color="Overweight")) +
#   geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
#   geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
#   geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
#   geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
#   geom_line(aes(y=death, color="Death")) +
#   ggtitle("Wegovy: 5 Year Change in White Population") +
#   labs(x = "Months",
#        y = "Pop Count",
#        color = "State") + 
#   scale_color_manual(values = colors) + scale_y_continuous(breaks=c(0,20000,40000,60000)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

###############################################################################
#Wegovy model--Black Population
###############################################################################

markovProbs1 = c(0.99805,0,0,0,0,0,0,
                 0,0.99789,0,0,0,0,0,
                 0,0,0.99739,0.136,0,0,0,
                 0,0,0,0.86056,0.136,0,0,
                 0,0,0,0,0.85958,0.068,0,
                 0,0,0,0,0,0.92633,0,
                 0.00195,0.00211,0.00261,0.00344,0.00442,0.00567,1.0)

markovProbs2 = c(0.99781,0,0,0,0,0,0,
                 0.00024,0.99765,0,0,0,0,0,
                 0,0.00024,0.99715,0,0,0,0,
                 0,0,0.00024,0.99632,0,0,0,
                 0,0,0,0.00024,0.99546,0,0,
                 0,0,0,0,0.00012,0.99433,0,
                 0.00195,0.00211,0.00261,0.00344,0.00442,0.00567,1.0)


M1 = matrix(markovProbs1,nrow=7,ncol=7,byrow=TRUE)
M2 = matrix(markovProbs2,nrow=7,ncol=7,byrow=TRUE)

utilityValues = c(0.81,0.81,0.79,0.76, 0.71, 0.66, 0.0) 
costValue = 1349.02

states=c(1,2,3,4,5,6,7)
nMonths1 = 12
nMonths2 = 48
totalMonths = nMonths1 + nMonths2


stateTraj = matrix(0,nrow=totalMonths,ncol=7)
initStateVec = c(0,0,0,8318,4165,4165,0)
currStateVec = initStateVec

#Initalize QALY vector and cost vector
totalQALYs3 = 0
totalCosts3 = 0
QALY_W_list3 = list()
Cost_W_list3 = list()

for (t in 1:nMonths) {
  stateTraj[t,] = currStateVec
  
  #Calculate this months QALYs
  currQALYs3 = sum(currStateVec*utilityValues)
  totalQALYs3 = totalQALYs3 + currQALYs3
  QALY_W_list3[t] = totalQALYs3
  
  #Calculate this months costs
  currCosts3 = sum(currStateVec*costValue)
  totalCosts3 = totalCosts3 + currCosts3
  Cost_W_list3[t] = totalCosts3
  
  if (t <= nMonths1){
    nextStateVec = M1 %*% currStateVec
  } else {
    nextStateVec = M2 %*% currStateVec
  }
  #draw next state
  #transitionProbs = M[,currState]
  #nextState = sample(states,1,prob=transitionProbs)
  #print(nextState)
  
  #update current state
  currStateVec = nextStateVec
}
print(stateTraj)

res3 = as.data.frame(stateTraj)
names(res3) = c('normal_weight',
               'overweight',
               'obesity_class_I',
               'obesity_class_II',
               'obesity_class_III_I',
               'obesity_class_III_II',
               'death')
res3$time = seq(1:totalMonths)

# Plot the results!

# colors <- c("Normal Weight"="#db7093","Overweight"="#40e0d0",
#             "Obesity Class I"="#eee8aa","Obesity Class II" = "blue", 
#             "Obesity Class III.I" = "red", 
#             "Obesity Class III.II" = "orange",
#             "Death" = "purple")
# 
# ggplot(data=res3, aes(x=time)) + 
#   #geom_line(aes(y=normal_weight, color="Normal Weight")) +
#   #geom_line(aes(y=overweight, color="Overweight")) +
#   geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
#   geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
#   geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
#   geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
#   geom_line(aes(y=death, color="Death")) +
#   ggtitle("Wegovy: 5 Year Change in Black Population") +
#   labs(x = "Months",
#        y = "Pop Count",
#        color = "State") +
#   scale_color_manual(values = colors) +
#   scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000),limits=c(0, 10000)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

###############################################################################
#Surgery model--White Population
###############################################################################

markovProbs1 = c(0.99903,0,0,0,0,0,0,
                 0,0.99895,0,0,0,0,0,
                 0,0,0.99873,0.16,0,0,0,
                 0,0,0,0.83835,0.16,0,0,
                 0,0,0,0,0.83792,0.08,0,
                 0,0,0,0,0,0.91735,0,
                 0.00097,0.00105,0.00127,0.00165,0.00208,0.00265,1.0)

markovProbs2 = c(0.99652,0,0,0,0,0,0,
                 0.0026,0.99644,0,0,0,0,0,
                 0,0.0026,0.99622,0,0,0,0,
                 0,0,0.0026,0.99584,0,0,0,
                 0,0,0,0.0026,0.9967,0,0,
                 0,0,0,0,0.0013,0.99744,0,
                 0.00088,0.00096,0.00118,0.00156,0.002,0.00256,1.0)


M1 = matrix(markovProbs1,nrow=7,ncol=7,byrow=TRUE)
M2 = matrix(markovProbs2,nrow=7,ncol=7,byrow=TRUE)

utilityValues = c(0.81,0.81,0.79,0.76, 0.71, 0.66, 0.0) 
utilityValues_month1 = utilityValues - 0.0619525
costValue = 17814.96

states=c(1,2,3,4,5,6,7)
nMonths1 = 12
nMonths2 = 48
totalMonths = nMonths1 + nMonths2


stateTraj = matrix(0,nrow=totalMonths,ncol=7)
initStateVec = c(0,0,0,52055,25409,25409,0)
currStateVec = initStateVec

#Initalize QALY vector and cost vector
totalQALYs4 = 0
totalCosts4 = 0
QALY_S_list4 = list()
Cost_S_list4 = list()


##Set loop 
for (t in 1:nMonths) {
  stateTraj[t,] = currStateVec
  
  #Calculate this months QALYs
  if (t==1){
    currQALYs4 = sum(currStateVec*utilityValues_month1)
    totalQALYs4 = totalQALYs4 + currQALYs4
    QALY_S_list4[t] = totalQALYs4
  } else {
    currQALYs4 = sum(currStateVec*utilityValues)
    totalQALYs4 = totalQALYs4 + currQALYs4
    QALY_S_list4[t] = totalQALYs4
  }
  
  #Calculate this months costs
  if (t==1){
    currCosts4 = sum(currStateVec*costValue)
    totalCosts4 = totalCosts4 + currCosts4
    Cost_S_list4[t] = totalCosts4
  } else {
    totalCosts4 = totalCosts4 + 0
  }
  
  if (t <= nMonths1){
    nextStateVec = M1 %*% currStateVec
  } else {
    nextStateVec = M2 %*% currStateVec
  }
  #draw next state
  #transitionProbs = M[,currState]
  #nextState = sample(states,1,prob=transitionProbs)
  #print(nextState)
  
  #update current state
  currStateVec = nextStateVec
}
print(stateTraj)

res4 = as.data.frame(stateTraj)
names(res4) = c('normal_weight',
               'overweight',
               'obesity_class_I',
               'obesity_class_II',
               'obesity_class_III_I',
               'obesity_class_III_II',
               'death')
res4$time = seq(1:totalMonths)

# Plot the results!

# colors <- c("Normal Weight"="#db7093","Overweight"="#40e0d0",
#             "Obesity Class I"="#eee8aa","Obesity Class II" = "blue", 
#             "Obesity Class III.I" = "red", 
#             "Obesity Class III.II" = "orange",
#             "Death" = "purple")
# 
# ggplot(data=res4, aes(x=time)) + 
#   #geom_line(aes(y=normal_weight, color="Normal Weight")) +
#   #geom_line(aes(y=overweight, color="Overweight")) +
#   geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
#   geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
#   geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
#   geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
#   geom_line(aes(y=death, color="Death")) +
#   ggtitle("Bariatric Surgery: 5 Year Change in White Population") +
#   labs(x = "Months",
#        y = "Pop Count",
#        color = "State") + 
#   scale_color_manual(values = colors) + scale_y_continuous(breaks=c(0,20000,40000,60000)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

###############################################################################
#Surgery model--Black Population
###############################################################################

markovProbs1 = c(0.99849,0,0,0,0,0,0,
                 0,0.99838,0,0,0,0,0,
                 0,0,0.99806,0.144,0,0,0,
                 0,0,0,0.85351,0.144,0,0,
                 0,0,0,0,0.85287,0.072,0,
                 0,0,0,0,0,0.92405,0,
                 0.00151,0.00162,0.00194,0.00249,0.00313,0.00395,1.0)

markovProbs2 = c(0.99612,0,0,0,0,0,0,
                 0.0026,0.99601,0,0,0,0,0,
                 0,0.0026,0.99568,0,0,0,0,
                 0,0,0.0026,0.99513,0,0,0,
                 0,0,0,0.0026,0.99579,0,0,
                 0,0,0,0,0.0013,0.99627,0,
                 0.00128,0.00139,0.00172,0.00227,0.00291,0.00373,1.0)


M1 = matrix(markovProbs1,nrow=7,ncol=7,byrow=TRUE)
M2 = matrix(markovProbs2,nrow=7,ncol=7,byrow=TRUE)

utilityValues = c(0.81,0.81,0.79,0.76, 0.71, 0.66, 0.0) 
utilityValues_month1 = utilityValues - 0.0640875
costValue = 18255.748

states=c(1,2,3,4,5,6,7)
nMonths1 = 12
nMonths2 = 48
totalMonths = nMonths1 + nMonths2


stateTraj = matrix(0,nrow=totalMonths,ncol=7)
initStateVec = c(0,0,0,8318,4165,4165,0)
currStateVec = initStateVec

#Initalize QALY vector and cost vector
totalQALYs5 = 0
totalCosts5 = 0
QALY_S_list5 = list()
Cost_S_list5 = list()

##Set loop 
for (t in 1:nMonths) {
  stateTraj[t,] = currStateVec
  
  #Calculate this months QALYs
  if (t==1){
    currQALYs5 = sum(currStateVec*utilityValues_month1)
    totalQALYs5 = totalQALYs5 + currQALYs5
    QALY_S_list5[t] = totalQALYs5
  } else {
    currQALYs5 = sum(currStateVec*utilityValues)
    totalQALYs5 = totalQALYs5 + currQALYs5
    QALY_S_list5[t] = totalQALYs5
  }
  
  #Calculate this months costs
  if (t==1){
    currCosts5 = sum(currStateVec*costValue)
    totalCosts5 = totalCosts5 + currCosts5
    Cost_S_list5[t] = totalCosts5
  } else {
    totalCosts5 = totalCosts5 + 0
  }
  
  if (t <= nMonths1){
    nextStateVec = M1 %*% currStateVec
  } else {
    nextStateVec = M2 %*% currStateVec
  }
  #draw next state
  #transitionProbs = M[,currState]
  #nextState = sample(states,1,prob=transitionProbs)
  #print(nextState)
  
  #update current state
  currStateVec = nextStateVec
}
print(stateTraj)

res5 = as.data.frame(stateTraj)
names(res5) = c('normal_weight',
               'overweight',
               'obesity_class_I',
               'obesity_class_II',
               'obesity_class_III_I',
               'obesity_class_III_II',
               'death')
res5$time = seq(1:totalMonths)

# # Plot the results!
# 
# colors <- c("Normal Weight"="#db7093","Overweight"="#40e0d0",
#             "Obesity Class I"="#eee8aa","Obesity Class II" = "blue", 
#             "Obesity Class III.I" = "red", 
#             "Obesity Class III.II" = "orange",
#             "Death" = "purple")
# 
# ggplot(data=res5, aes(x=time)) + 
#   #geom_line(aes(y=normal_weight, color="Normal Weight")) +
#   #geom_line(aes(y=overweight, color="Overweight")) +
#   geom_line(aes(y=obesity_class_I, color="Obesity Class I")) +
#   geom_line(aes(y=obesity_class_II, color="Obesity Class II")) +
#   geom_line(aes(y=obesity_class_III_I, color="Obesity Class III.I")) + 
#   geom_line(aes(y=obesity_class_III_II, color="Obesity Class III.II")) + 
#   geom_line(aes(y=death, color="Death")) +
#   ggtitle("Bariatric Surgery: 5 Year Change in Black Population") +
#   labs(x = "Months",
#        y = "Pop Count",
#        color = "State") +
#   scale_color_manual(values = colors) +
#   scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000),limits=c(0, 10000)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black"))

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


###Calculate ICERS

###Compare Wegovy vs. SQ########################################################

#White population
ICER1 = totalCosts2 / (totalQALYs2 - totalQALYs)

##Black population
ICER2 = totalCosts3 / (totalQALYs3 - totalQALYs1)


###Compare Surgery vs. SQ#######################################################

#White population
ICER3 = totalCosts4 / (totalQALYs4 - totalQALYs)

##Black population
ICER4 = totalCosts5 / (totalQALYs5 - totalQALYs1)


###Compare Wegovy vs. Surgery##################################################

#White population
ICER5 = (totalCosts2 - totalCosts4) / (totalQALYs2 - totalQALYs4)

##Black population
ICER6 = (totalCosts3 - totalCosts5)  / (totalQALYs3 - totalQALYs5)

