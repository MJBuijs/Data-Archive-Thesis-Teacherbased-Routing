set.seed(84)

# Determine the avarage difficulty of each module in each design 
## ---------------------------------------------------------------------------------------------------------------------------------

# Create storage
Means <- matrix(ncol = 22, nrow = 100)
Variances <- matrix(ncol = 22, nrow = 100)
M_L5 <- matrix(ncol = 1, nrow = 100)
M_U5 <- matrix(ncol = 1, nrow = 100)
M_L3 <- matrix(ncol = 1, nrow = 100)
M_M3 <- matrix(ncol = 1, nrow = 100)
M_U3 <- matrix(ncol = 1, nrow = 100)
Var_L5 <- matrix(ncol = 1, nrow = 100)
Var_U5 <- matrix(ncol = 1, nrow = 100)
Var_L3 <- matrix(ncol = 1, nrow = 100)
Var_M3 <- matrix(ncol = 1, nrow = 100)
Var_U3 <- matrix(ncol = 1, nrow = 100)

# Using CBS data: obtain recommendations that are later matched to the abilities 
per_group <- round(tabelx[,3]/100*n)
d <- n - sum(per_group)
docent_advies <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9), c((per_group[1,1]+d), per_group[2,1], per_group[3,1], per_group[4,1], (per_group[5,1]), per_group[6,1], per_group[7,1], per_group[8,1], per_group[9,1]))

# for loop; create 100 datasets, look what would be optimal treshold and what average ability is of each proportion that will be designed to a module (to match beta) 
for (i in 1:100){
  # Draw abilities, order abilities, provide lowest x% with a vmbo-bb recommendation, next x% with bb/kb recommendation (from "docent_advies")
  thetas <- as.data.frame(thetas <- rnorm(n,mu,sd))
  thetas <- thetas[order(thetas$thetas),]
  thetas <- cbind.data.frame(thetas, docent_advies)
  
  # obtain mean and variance of lowest/highest 50% and lowest/middle/upper 30% for modules in second stage 
  M_L5[i] <- mean(thetas$thetas[1:(n/2)])
  M_U5[i] <- mean(thetas$thetas[(n/2):nrow(thetas)])
  
  M_L3[i] <- mean(thetas$thetas[1:(round(n/3))])
  M_M3[i] <- mean(thetas$thetas[(round(n/3)):(round(n/3*2))])
  M_U3[i] <- mean(thetas$thetas[(round(n/3*2)):nrow(thetas)])
  
  Var_L5[i] <- var(thetas$thetas[1:(n/2)])
  Var_U5[i] <- var(thetas$thetas[(n/2):nrow(thetas)])
  Var_L3[i] <- var(thetas$thetas[1:(round(n/3))])
  Var_M3[i] <- var(thetas$thetas[(round(n/3)):(round(n/3*2))])
  Var_U3[i] <- var(thetas$thetas[(round(n/3*2)):nrow(thetas)])
  
  # allocate students and obtain mean/var of abilities of students allocated to a certain module in the first stage 
  theta <- Allocation(thetas, grens_23)
  Means[i,1] <- mean(theta[theta$module == 1,]$theta)
  Means[i,2] <- mean(theta[theta$module == 2,]$theta)
  Variances[i,1]  <- var(theta[theta$module == 1,]$theta)
  Variances[i,2]  <- var(theta[theta$module == 2,]$theta)
  
  # allocate students and obtain mean/var of abilities of students allocated to a certain module
  theta <- Allocation(thetas, grens_33)
  Means[i,3] <- mean(theta[theta$module == 1,]$theta)
  Means[i,4] <- mean(theta[theta$module == 2,]$theta) 
  Means[i,5] <- mean(theta[theta$module == 3,]$theta)
  Variances[i,3]  <- var(theta[theta$module == 1,]$theta)
  Variances[i,4]  <- var(theta[theta$module == 2,]$theta)
  Variances[i,5]  <- var(theta[theta$module == 3,]$theta)
  
  # allocate students and obtain mean/var of abilities of students allocated to a certain module
  theta <- Allocation(thetas, grens_43)
  Means[i,6] <- mean(theta[theta$module == 1,]$theta)
  Means[i,7] <- mean(theta[theta$module == 2,]$theta) 
  Means[i,8] <- mean(theta[theta$module == 3,]$theta)
  Means[i,9] <- mean(theta[theta$module == 4,]$theta)
  Variances[i,6]  <- var(theta[theta$module == 1,]$theta)
  Variances[i,7]  <- var(theta[theta$module == 2,]$theta)
  Variances[i,8]  <- var(theta[theta$module == 3,]$theta)
  Variances[i,9]  <- var(theta[theta$module == 4,]$theta)
 
  # allocate students and obtain mean/var of abilities of students allocated to a certain module 
  theta <- Allocation(thetas, grens_53)
  Means[i,10]<- mean(theta[theta$module == 1,]$theta) 
  Means[i,11]<- mean(theta[theta$module == 2,]$theta)
  Means[i,12]<- mean(theta[theta$module == 3,]$theta)
  Means[i,13]<- mean(theta[theta$module == 4,]$theta)
  Means[i,14]<- mean(theta[theta$module == 5,]$theta)
  Variances[i,10]  <- var(theta[theta$module == 1,]$theta)
  Variances[i,11]  <- var(theta[theta$module == 2,]$theta)
  Variances[i,12]  <- var(theta[theta$module == 3,]$theta)
  Variances[i,13]  <- var(theta[theta$module == 4,]$theta)
  Variances[i,14]  <- var(theta[theta$module == 5,]$theta)

  # allocate students and obtain mean/var of abilities of students allocated to a certain module
  theta <- Allocation(thetas, grens_83)
  Means[i,15]<- mean(theta[theta$module == 1,]$theta) 
  Means[i,16]<- mean(theta[theta$module == 2,]$theta)
  Means[i,17]<- mean(theta[theta$module == 3,]$theta)
  Means[i,18]<- mean(theta[theta$module == 4,]$theta)
  Means[i,19]<- mean(theta[theta$module == 5,]$theta)
  Means[i,20]<- mean(theta[theta$module == 6,]$theta)
  Means[i,21]<- mean(theta[theta$module == 7,]$theta)
  Means[i,22]<- mean(theta[theta$module == 8,]$theta)
  Variances[i,15]  <- var(theta[theta$module == 1,]$theta)
  Variances[i,16]  <- var(theta[theta$module == 2,]$theta)
  Variances[i,17]  <- var(theta[theta$module == 3,]$theta)
  Variances[i,18]  <- var(theta[theta$module == 4,]$theta)
  Variances[i,19]  <- var(theta[theta$module == 5,]$theta)
  Variances[i,20]  <- var(theta[theta$module == 6,]$theta)
  Variances[i,21]  <- var(theta[theta$module == 7,]$theta)
  Variances[i,22]  <- var(theta[theta$module == 8,]$theta)
}


# take mean ove 100 iteration to use in final analysis/simulation
M_L5 <- round(mean(M_L5),2)
M_U5 <- round(mean(M_U5),2)
Var_L5 <- round(mean(Var_L5),2)
Var_U5 <- round(mean(Var_U5),2)

M_L3 <- round(mean(M_L3),2)
M_M3 <- round(mean(M_M3),2)
M_U3 <- round(mean(M_U3),2)
Var_L3 <- round(mean(Var_L3),2)
Var_M3 <- round(mean(Var_M3),2)
Var_U3 <- round(mean(Var_U3),2)


M_m1_23 <- round(mean(Means[,1]),2)
M_m2_23 <- round(mean(Means[,2]),2)

M_m1_33 <- round(mean(Means[,3]),2)
M_m2_33 <- round(mean(Means[,4]),2)
M_m3_33 <- round(mean(Means[,5]),2)

M_m1_43 <- round(mean(Means[,6]),2)
M_m2_43 <- round(mean(Means[,7]),2)
M_m3_43 <- round(mean(Means[,8]),2)
M_m4_43 <- round(mean(Means[,9]),2)

M_m1_53 <- round(mean(Means[,10]),2)
M_m2_53 <- round(mean(Means[,11]),2)
M_m3_53 <- round(mean(Means[,12]),2)
M_m4_53 <- round(mean(Means[,13]),2)
M_m5_53 <-round(mean(Means[,14]),2)

M_m1_83 <- round(mean(Means[,15]),2)
M_m2_83 <-round(mean(Means[,16]),2)
M_m3_83 <- round(mean(Means[,17]),2)
M_m4_83 <- round(mean(Means[,18]),2)
M_m5_83 <- round(mean(Means[,19]),2)
M_m6_83 <- round(mean(Means[,20]),2)
M_m7_83 <- round(mean(Means[,21]),2)
M_m8_83 <- round(mean(Means[,22]),2)

var_L5 <- round(mean(Variances[,1]),2)
var_U5 <- round(mean(Variances[,2]),2)

var_L3 <- round(mean(Variances[,3]),2)
var_M3 <- round(mean(Variances[,4]),2)
var_U3 <- round(mean(Variances[,5]),2)

Var_m1_23 <- round(mean(Variances[,1]),2)
Var_m2_23 <- round(mean(Variances[,2]),2)

Var_m1_33 <- round(mean(Variances[,3]),2)
Var_m2_33 <- round(mean(Variances[,4]),2)
Var_m3_33 <- round(mean(Variances[,5]),2)

Var_m1_43 <- round(mean(Variances[,6]),2)
Var_m2_43 <- round(mean(Variances[,7]),2)
Var_m3_43 <- round(mean(Variances[,8]),2)
Var_m4_43 <-round(mean(Variances[,9]),2)

Var_m1_53 <-round(mean(Variances[,10]),2)
Var_m2_53 <- round(mean(Variances[,11]),2)
Var_m3_53 <-round(mean(Variances[,12]),2)
Var_m4_53 <- round(mean(Variances[,13]),2)
Var_m5_53 <- round(mean(Variances[,14]),2)

Var_m1_83 <-round(mean(Variances[,15]),2)
Var_m2_83 <-round(mean(Variances[,16]),2)
Var_m3_83 <-round(mean(Variances[,17]),2)
Var_m4_83 <-round(mean(Variances[,18]),2)
Var_m5_83 <-round(mean(Variances[,19]),2)
Var_m6_83 <-round(mean(Variances[,20]),2)
Var_m7_83 <-round(mean(Variances[,21]),2)
Var_m8_83 <-round(mean(Variances[,22]),2)

