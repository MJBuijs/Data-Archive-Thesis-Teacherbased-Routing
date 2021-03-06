rm(list = ls())
set.seed(42)

# source files containing functions, import CBS table etc. 
file.sources <- list.files(path = "SourceFiles", pattern="*.R", full.names=TRUE)
sapply(file.sources, source, .GlobalEnv)


## ---------------------------------------------------------------------------------------------------------------------------------
# Simulate and save iter datalists, with n examinees - Use parallel programming to decrease computational time 
registerDoParallel(cores = (detectCores() - 4)) 
Data_lijst <- foreach(k = 1:iter, .verbose=TRUE) %dopar%

    {
      Data_simulation1(k, n, mu, sd)
    }

save(Data_lijst, file = "Data/Datalist.RData")
registerDoSEQ() 

## ---------------------------------------------------------------------------------------------------------------------------------
# Do some checks

c <- bind_rows(Data_lijst)
summary(c)
cor(bind_rows(Data_lijst))

# Check if proportions are indeed equal to CBS data
Combined <- bind_rows(Data_lijst)
tabn <- Overeenkomst_tabel(Combined, type = "n")
prop_sim <- tabn[[1]]/sum(tabn[[1]])*100
tabel2 <- read_excel("Overig/tabel5.xlsx")
(prop_CBS <- tabel2/sum(tabel2)*100)
tabn/n*100

# Are there any cases for which the (estimated) ability overlaps between different track placement recommendations/optimal track placements?
which(lapply(Data_lijst, function(x) {
  (
    (max(x[x$plaatsing == 1,]$theta) <= min(x[x$plaatsing == 2,]$theta)) &
      (max(x[x$plaatsing == 2,]$theta) <= min(x[x$plaatsing == 3,]$theta)) &
          (max(x[x$plaatsing == 3,]$theta) <= min(x[x$plaatsing == 5,]$theta)) &
              ( max(x[x$plaatsing == 5,]$theta) <= min(x[x$plaatsing == 6,]$theta)) &
                  (max(x[x$plaatsing == 6,]$theta) <= min(x[x$plaatsing == 7,]$theta)) &
                     (max(x[x$plaatsing == 7,]$theta) <= min(x[x$plaatsing == 8,]$theta)) &
                         (max(x[x$plaatsing == 8,]$theta) <= min(x[x$plaatsing == 9,]$theta))
    )})==FALSE)

which(lapply(Data_lijst, function(x) {
  (
      (max(x[x$toets_advies == 1,]$EPST_est) <= min(x[x$toets_advies == 2,]$EPST_est)) &
      (max(x[x$toets_advies == 2,]$EPST_est) <= min(x[x$toets_advies == 3,]$EPST_est)) &
      (max(x[x$toets_advies == 3,]$EPST_est) <= min(x[x$toets_advies == 5,]$EPST_est)) &
      (max(x[x$toets_advies == 5,]$EPST_est) <= min(x[x$toets_advies == 6,]$EPST_est)) &
      (max(x[x$toets_advies == 6,]$EPST_est) <= min(x[x$toets_advies == 7,]$EPST_est)) &
      (max(x[x$toets_advies == 7,]$EPST_est) <= min(x[x$toets_advies == 8,]$EPST_est)) &
      (max(x[x$toets_advies == 8,]$EPST_est) <= min(x[x$toets_advies == 9,]$EPST_est))
  )})==FALSE)



