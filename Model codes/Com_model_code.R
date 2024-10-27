###Load matrix functions into R session
setwd("/Users/neetikulkarni/Desktop/Summer Efficiency Frontier Research")
source("matrix_functions.R")

###############################################################################
#Status quo model--White Population
###############################################################################

library(ggplot2)

# markovProbs = c(0.99142,0,0,0,
#                 0.0058,0.9939,0,0,
#                 0,0.00254,0.99543,0,
#                 0.00278,0.00356,0.00457,1.0)

#M = matrix(markovProbs,nrow=4,ncol=4,byrow=TRUE)
#print(M)
SQ_mr = c(0.0334,0.0428,0.0550,0)
M = SQ_matrix(SQ_mr,0.0127)

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

###############################################################################
#Status quo model--Black Population
###############################################################################

# markovProbs = c(0.99015,0,0,0,
#                 0.0058,0.99227,0,0,
#                 0,0.00254,0.99333,0,
#                 0.00405,0.00519,0.00667,1.0)
# M = matrix(markovProbs,nrow=4,ncol=4,byrow=TRUE)
# print(M)

SQ_mr = c(0.0487,0.0625,0.0803,0)
M = SQ_matrix(SQ_mr,0.0127)

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


###############################################################################
#Wegovy model--White Population
###############################################################################

# markovProbs1 = c(0.99912,0,0,0,0,0,0,
#                 0,0.99904,0,0,0,0,0,
#                 0,0,0.99882,0.136,0,0,0,
#                 0,0,0,0.86244,0.136,0,0,
#                 0,0,0,0,0.86200,0.068,0,
#                 0,0,0,0,0,0.92944,0,
#                 0.00088,0.00096,0.00118,0.00156,0.00200,0.00256,1.0)
# 
# markovProbs2 = c(0.99888,0,0,0,0,0,0,
#                  0.00024,0.9988,0,0,0,0,0,
#                  0,0.00024,0.99858,0,0,0,0,
#                  0,0,0.00024,0.9982,0,0,0,
#                  0,0,0,0.00024,0.99788,0,0,
#                  0,0,0,0,0.00012,0.99744,0,
#                  0.00088,0.00096,0.00118,0.00156,0.00200,0.00256,1.0)
                
WegovyMR_WY1 = c(0.0189,0.0205,0.0253,0.0334,0.0428,0.0550,0)
WegovyMR_WY25 = c(0.0189,0.0205,0.0253,0.0334,0.0428,0.0550,0)
M1 = InterventionY1_matrix(WegovyMR_WY1,0.34,0.56)
M2 = InterventionY25_matrix(WegovyMR_WY25,0.0012,0.56)
print(M1)

# M1 = matrix(markovProbs1,nrow=7,ncol=7,byrow=TRUE)
# M2 = matrix(markovProbs2,nrow=7,ncol=7,byrow=TRUE)

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


###############################################################################
#Wegovy model--Black Population
###############################################################################

# markovProbs1 = c(0.99872,0,0,0,0,0,0,
#                  0,0.99861,0,0,0,0,0,
#                  0,0,0.99828,0.136,0,0,0,
#                  0,0,0,0.86173,0.136,0,0,
#                  0,0,0,0,0.86109,0.068,0,
#                  0,0,0,0,0,0.92827,0,
#                  0.00128,0.00139,0.00172,0.00227,0.00291,0.00373,1.0)
# 
# markovProbs2 = c(0.99848,0,0,0,0,0,0,
#                  0.00024,0.99837,0,0,0,0,0,
#                  0,0.00024,0.99804,0,0,0,0,
#                  0,0,0.00024,0.99749,0,0,0,
#                  0,0,0,0.00024,0.99697,0,0,
#                  0,0,0,0,0.00012,0.99627,0,
#                  0.00128,0.00139,0.00172,0.00227,0.00291,0.00373,1.0)
# 
# 
# M1 = matrix(markovProbs1,nrow=7,ncol=7,byrow=TRUE)
# M2 = matrix(markovProbs2,nrow=7,ncol=7,byrow=TRUE)

WegovyMR_BY1 = c(0.0275,0.0299,0.0369,0.0487,0.0625,0.0803,0)
WegovyMR_BY25 = c(0.0275,0.0299,0.0369,0.0487,0.0625,0.0803,0)
M1 = InterventionY1_matrix(WegovyMR_BY1,0.34,0.555)
M2 = InterventionY25_matrix(WegovyMR_BY25,0.0012,0.555)

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

###############################################################################
#Surgery model--White Population
###############################################################################

# markovProbs1 = c(0.99917,0,0,0,0,0,0,
#                  0,0.9991,0,0,0,0,0,
#                  0,0,0.99888,0.16,0,0,0,
#                  0,0,0,0.83853,0.16,0,0,
#                  0,0,0,0,0.83811,0.08,0,
#                  0,0,0,0,0,0.91758,0,
#                  0.00083,0.00090,0.00112,0.00147,0.00189,0.00242,1.0)
# 
# markovProbs2 = c(0.99657,0,0,0,0,0,0,
#                  0.0026,0.9965,0,0,0,0,0,
#                  0,0.0026,0.99628,0,0,0,0,
#                  0,0,0.0026,0.99593,0,0,0,
#                  0,0,0,0.0026,0.99681,0,0,
#                  0,0,0,0,0.0013,0.99758,0,
#                  0.00083,0.00090,0.00112,0.00147,0.00189,0.00242,1.0)
# 
# 
# M1 = matrix(markovProbs1,nrow=7,ncol=7,byrow=TRUE)
# M2 = matrix(markovProbs2,nrow=7,ncol=7,byrow=TRUE)

SurgeryMR_WY1 = c(0.0189,0.0205,0.0253,0.0334,0.0428,0.0550,0)
SurgeryMR_WY25 = c(0.0189,0.0205,0.0253,0.0334,0.0428,0.0550,0)
M1 = InterventionY1_matrix(SurgeryMR_WY1,0.4,0.52)
M2 = InterventionY25_matrix(SurgeryMR_WY25,0.0065,0.52)

utilityValues = c(0.81,0.81,0.79,0.76, 0.71, 0.66, 0.0) 
utilityValues_month1 = utilityValues - 0.0707525
costValue = 20432.56

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

###############################################################################
#Surgery model--Black Population
###############################################################################

# markovProbs1 = c(0.99872,0,0,0,0,0,0,
#                  0,0.99861,0,0,0,0,0,
#                  0,0,0.99828,0.144,0,0,0,
#                  0,0,0,0.85373,0.144,0,0,
#                  0,0,0,0,0.85309,0.072,0,
#                  0,0,0,0,0,0.92427,0,
#                  0.00128,0.00139,0.00172,0.00227,0.00291,0.00373,1.0)
# 
# markovProbs2 = c(0.99612,0,0,0,0,0,0,
#                  0.0026,0.99601,0,0,0,0,0,
#                  0,0.0026,0.99568,0,0,0,0,
#                  0,0,0.0026,0.99513,0,0,0,
#                  0,0,0,0.0026,0.99579,0,0,
#                  0,0,0,0,0.0013,0.99627,0,
#                  0.00128,0.00139,0.00172,0.00227,0.00291,0.00373,1.0)
# 
# 
# M1 = matrix(markovProbs1,nrow=7,ncol=7,byrow=TRUE)
# M2 = matrix(markovProbs2,nrow=7,ncol=7,byrow=TRUE)

SurgeryMR_BY1 = c(0.0275,0.0299,0.0369,0.0487,0.0625,0.0803,0)
SurgeryMR_BY25 = c(0.0275,0.0299,0.0369,0.0487,0.0625,0.0803,0)
M1 = InterventionY1_matrix(SurgeryMR_BY1,0.36,0.55)
M2 = InterventionY25_matrix(SurgeryMR_BY25,0.0065,0.55)

utilityValues = c(0.81,0.81,0.79,0.76, 0.71, 0.66, 0.0) 
utilityValues_month1 = utilityValues - 0.0728875
costValue = 20873.35

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

