rm(list = ls())
set.seed(84)

# source files containing the functions
file.sources <- list.files(path = "Functions", pattern="*.R", full.names=TRUE)
sapply(file.sources, source, .GlobalEnv)

# load results
file.load <- list.files(path = "Data", pattern = "*.RData", full.names = TRUE)
sapply(file.load, load, .GlobalEnv)

## ---------------------------------------------------------------------------------------------------------------------------------
# Given these datasets, for each proposed design, allocate students to a module (sequential, not using more cores)
Data_allocated_lijst_23 <- lapply(Data_lijst, Allocation, grens_23)
Data_allocated_lijst_33 <- lapply(Data_lijst, Allocation, grens_33)
Data_allocated_lijst_43 <- lapply(Data_lijst, Allocation, grens_43)
Data_allocated_lijst_53 <- lapply(Data_lijst, Allocation, grens_53)
Data_allocated_lijst_83 <- lapply(Data_lijst, Allocation, grens_83)

## ---------------------------------------------------------------------------------------------------------------------------------
## Given allocation of modules, what is estimated to be the ability + advice? 
registerDoParallel(cores = (detectCores() - 4)) #use more than the standard number of cores to increase computational speed

set.seed(84)
Results_23 <- foreach(k = 1:length(Data_allocated_lijst_23), .verbose=TRUE) %dopar%
  {Sim_design1(Data_allocated_lijst_23[[k]], modules_23, trans_23, type = "23", i = k)}
save(Results_23, file = "Results/Results23.RData")
Sys.sleep(300)

set.seed(126)
Results_33 <- foreach(k = 1:length(Data_allocated_lijst_33), .verbose=TRUE) %dopar%
  {Sim_design1(Data_allocated_lijst_33[[k]], modules_33, trans_33, type = "33", i = k)}
save(Results_33, file = "Results/Results33.RData")
Sys.sleep(300)

set.seed(168)
Results_43 <- foreach(k = 1:length(Data_allocated_lijst_43), .verbose=TRUE) %dopar%
  {Sim_design1(Data_allocated_lijst_43[[k]], modules_43, trans_43, type = "43", i = k)}
save(Results_43, file = "Results/Results43.RData")
Sys.sleep(300)

set.seed(210)
Results_53 <- foreach(k = 1:length(Data_allocated_lijst_53), .verbose=TRUE) %dopar%
  {Sim_design1(Data_allocated_lijst_53[[k]], modules_53, trans_53, type = "53", i = k)}
save(Results_53, file = "Results/Results53.RData")
Sys.sleep(300)

set.seed(252)
Results_83 <- foreach(k = 1:length(Data_allocated_lijst_83), .verbose=TRUE) %dopar%
  {Sim_design1(Data_allocated_lijst_83[[k]], modules_83, trans_83, type = "83", i = k)}
save(Results_83, file = "Results/Results83.RData")
Sys.sleep(300)

registerDoSEQ() #back to sequential programming

## ---------------------------------------------------------------------------------------------------------------------------------

# Some checks
Results <- list(Results_23, Results_33, Results_43, Results_53, Results_83) #Combine results
# Are students indeed allocated to the correct starting module? 
startmodule <- function(test){
  sum <- sum(test$module != test$startmod) #should be equal in all cases
  if (sum != 0)
    return(sum)
}
lapply(Results, lapply, startmodule) #all good

# Are there any cases for which the estimated ability overlaps between different track placement recommendations?
check <- (lapply(Results, lapply, function(x) {
  (
    (max(x[x$design_advies == 1,]$theta_estimate) <= min(x[x$design_advies == 2,]$theta_estimate)) &
      (max(x[x$design_advies == 2,]$theta_estimate) <= min(x[x$design_advies == 3,]$theta_estimate)) &
      (max(x[x$design_advies == 3,]$theta_estimate) <= min(x[x$design_advies == 5,]$theta_estimate)) &
      (max(x[x$design_advies == 5,]$theta_estimate) <= min(x[x$design_advies == 6,]$theta_estimate)) &
      (max(x[x$design_advies == 6,]$theta_estimate) <= min(x[x$design_advies == 7,]$theta_estimate)) &
      (max(x[x$design_advies == 7,]$theta_estimate) <= min(x[x$design_advies == 8,]$theta_estimate)) &
      (max(x[x$design_advies == 8,]$theta_estimate) <= min(x[x$design_advies == 9,]$theta_estimate))
  )}))

which(check[[1]]==FALSE)
which(check[[2]]==FALSE)
which(check[[3]]==FALSE)
which(check[[4]]==FALSE)
which(check[[5]]==FALSE)


## ---------------------------------------------------------------------------------------------------------------------------------
# Combine results
Combined_23 <- bind_rows(Results_23) %>% 
  mutate(mod = "2 modules")
Combined_33 <- bind_rows(Results_33)%>% 
  mutate(mod = "3 modules")
Combined_43 <- bind_rows(Results_43)%>% 
  mutate(mod = "4 modules")
Combined_53 <- bind_rows(Results_53)%>% 
  mutate(mod = "5 modules")
Combined_83 <- bind_rows(Results_83)%>% 
  mutate(mod = "8 modules")

save("Combined_23", "Combined_33", "Combined_43", "Combined_53", "Combined_83", file = "Results/Combined.RData")


