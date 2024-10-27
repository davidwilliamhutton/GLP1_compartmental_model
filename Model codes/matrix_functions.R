tranProb_mr=function(mr){
  P=1-exp(-(mr/12))
  return(P)
}

tranProb=function(r){
  P=1-exp(-(r))
  return(P)
}

# Function to create a 4x4 matrix for SQ intervention

SQ_matrix <- function(mr_list,BMI_change) {
  # Create an empty 4x4 matrix (filled with zeros initially)
  mat = matrix(0, nrow=4, ncol=4,byrow=TRUE)
  mr_list = tranProb_mr(mr_list)
  mat[4,] = mr_list
  BMI_rate = 1/(2.5/BMI_change)
  BMI_prob = tranProb(BMI_rate)
  mat[2,1] = BMI_prob
  mat[3,2] = BMI_prob / 2
  mat[1,1] = 1-mat[4,1]-mat[2,1]
  mat[2,2] = 1-mat[4,2]-mat[3,2]
  mat[3,3] = 1-mat[4,3]
  mat[4,4] = 1.0
  return(mat)
}

# Example usage
my_list <- c(0.0334,0.0428,0.0550,0)
my_matrix <- SQ_matrix(my_list,0.0127)

# Print the matrix
print(my_matrix)


markovProbs1 = c(0.99912,0,0,0,0,0,0,
                 0,0.99904,0,0,0,0,0,
                 0,0,0.99882,0.136,0,0,0,
                 0,0,0,0.86244,0.136,0,0,
                 0,0,0,0,0.86200,0.068,0,
                 0,0,0,0,0,0.92944,0,
                 0.00088,0.00096,0.00118,0.00156,0.00200,0.00256,1.0)
markovProbs2 = c(0.99888,0,0,0,0,0,0,
                 0.00024,0.9988,0,0,0,0,0,
                 0,0.00024,0.99858,0,0,0,0,
                 0,0,0.00024,0.9982,0,0,0,
                 0,0,0,0.00024,0.99788,0,0,
                 0,0,0,0,0.00012,0.99744,0,
                 0.00088,0.00096,0.00118,0.00156,0.00200,0.00256,1.0)


# Function to create a 7x7 matrix for first year of intervention
InterventionY1_matrix <- function(mr_list,BMI_change,HR) {
  # Create an empty 7x7 matrix (filled with zeros initially)
  mat = matrix(0, nrow=7, ncol=7,byrow=TRUE)
  mr_list = HR*mr_list
  mr_list = tranProb_mr(mr_list)
  mat[7,] = mr_list
  BMI_rate = 1/(2.5/BMI_change)
  BMI_prob = tranProb(BMI_rate)
  mat[3,4] = BMI_prob
  mat[4,5] = BMI_prob
  mat[5,6] = BMI_prob / 2
  mat[1,1] = 1-mat[7,1]
  mat[2,2] = 1-mat[7,2]
  mat[3,3] = 1-mat[7,3]
  mat[4,4] = 1-mat[7,4]-mat[3,4]
  mat[5,5] = 1-mat[7,5]-mat[4,5]
  mat[6,6] = 1-mat[7,6]-mat[6,6]
  mat[7,7] = 1.0
  return(mat)
}

# Function to create a 7x7 matrix for first year of intervention
InterventionY25_matrix <- function(mr_list,BMI_change,HR) {
  # Create an empty 7x7 matrix (filled with zeros initially)
  mat = matrix(0, nrow=7, ncol=7,byrow=TRUE)
  mr_list = HR*mr_list
  mr_list = tranProb_mr(mr_list)
  mat[7,] = mr_list
  BMI_rate = 1/(2.5/BMI_change)
  BMI_prob = tranProb(BMI_rate)
  mat[2,1] = BMI_prob
  mat[3,2] = BMI_prob
  mat[4,3] = BMI_prob
  mat[5,4] = BMI_prob
  mat[6,5] = BMI_prob / 2
  mat[1,1] = 1-mat[7,1]-mat[2,1]
  mat[2,2] = 1-mat[7,2]-mat[3,2]
  mat[3,3] = 1-mat[7,3]-mat[4,3]
  mat[4,4] = 1-mat[7,4]-mat[5,4]
  mat[5,5] = 1-mat[7,5]-mat[6,5]
  mat[6,6] = 1-mat[7,6]
  mat[7,7] = 1.0
  return(mat)
}



