######
# F4. Function Sim_design()
#####


## ---------------------------------------------------------------------------------------------------------------------------------

Sim_design1 <- function(data, modules, trans, type = c("23", "33", "43", "53", "83"), i){
  n <- nrow(data)
  # Create item bank based on design
  if (type == "23")
  {it.MST <- rbind(mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m1_23, Var_m1_23), seed = i),
                   mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m2_23, Var_m2_23), seed = i),
                   mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_L3, var_L3), seed = i),
                   mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_M3, var_M3), seed = i),
                   mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_U3, var_U3), seed = i)
                   )}
  else if (type == "33")
  {  it.MST <- rbind(mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m1_33, Var_m1_33), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m2_33, Var_m2_33), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m3_33, Var_m3_33), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_L3, var_L3), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_M3, var_M3), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_U3, var_U3), seed = i)
                     )}
  else if (type == "43")
  {  it.MST <- rbind(mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m1_43, Var_m1_43), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m2_43, Var_m2_43), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m3_43, Var_m3_43), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m4_43, Var_m4_43), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_L3, var_L3), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_M3, var_M3), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_U3, var_U3), seed = i)
                     )}
  else if (type == "53")
  {  it.MST <- rbind(mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m1_53, Var_m1_53), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m2_53, Var_m2_53), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m3_53, Var_m3_53), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m4_53, Var_m4_53), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m5_53, Var_m5_53), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_L3, var_L3), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_M3, var_M3), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_U3, var_U3), seed = i)
                     )}
  else if (type == "83")
  {  it.MST <- rbind(mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m1_83, Var_m1_83), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m2_83, Var_m2_83), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m3_83, Var_m3_83), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m4_83, Var_m4_83), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m5_83, Var_m5_83), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m6_83, Var_m5_83), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m7_83, Var_m7_83), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_m8_83, Var_m8_83), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_L3, var_L3), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_M3, var_M3), seed = i),
                     mstR::genDichoMatrix(47, model = "1PL", bPrior = c("norm", M_U3, var_U3), seed = i)
                     )}
  it.MST <- as.matrix(it.MST)
  
  # Create storage
  theta_estimate <- matrix(ncol = 1, nrow = n)
  SE_test_prov <- matrix(ncol = 1, nrow = n)
  SE_test <- matrix(ncol=1, nrow=n)
  startmod <- matrix(ncol=1, nrow=n)
  secmod <- matrix(ncol=1, nrow=n)
  prov <- matrix(ncol=1, nrow=n)
  
  # estimate ability for each student given true ability, item bank, available modules etc.
for (j in 1:n) {
x <- mstR::randomMST(trueTheta = data[j,1], itemBank = it.MST, modules = modules, transMatrix = trans, start = list(fixModule = data[j,11], seed = j),
                       genSeed = j, test = list(method = "WL"), final = list(method = "WL"))
  theta_estimate[j] <- x$thFinal #save final theta estimate
  SE_test_prov[j] <- x$seProv[1]
  SE_test[j] <- x$seProv[2]
  startmod[j] <- x$selected.modules[1]
  secmod[j] <- x$selected.modules[2]
  prov[j] <- x$thetaProv[1]

}
data <- cbind(data, startmod, secmod, prov, SE_test_prov, theta_estimate, SE_test)

# Provide each student with a track recommendation 
data <- data[order(data$theta_estimate),]
per_group <- round(tabelx[,3]/100*n)
d <- n - sum(per_group)
design_advies <-
  rep(
    c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    c((per_group[1] + d),
      per_group[2],
      per_group[3],
      per_group[4],
      per_group[5],
      per_group[6],
      per_group[7],
      per_group[8],
      per_group[9]
    )
  )
data <- cbind.data.frame(data, design_advies)

# Calculate differences between recommendations
toets_design_dif <- as.numeric(as.character(data$toets_advies)) - as.numeric(as.character(data$design_advies))
plaatsing_design_dif <- as.numeric(as.character(data$plaatsing)) - as.numeric(as.character(data$design_advies))
data <- cbind.data.frame(data, toets_design_dif, plaatsing_design_dif)
data <- data[order(data$id),]

# Factorize 
data$startmod <- as.factor(data$startmod)
data$secmod <- as.factor(data$secmod)
data$design_advies <- as.factor(data$design_advies)
data$toets_design_dif <- as.factor(data$toets_design_dif)
data$plaatsing_design_dif <- as.factor(data$plaatsing_design_dif)

return(data)
}

## ---------------------------------------------------------------------------------------------------------------------------------
