rm(list = ls())
set.seed(42)

# source files containing the functions
file.sources <- list.files(path = "SourceFiles", pattern="*.R", full.names=TRUE)
sapply(file.sources, source, .GlobalEnv)

# load results
load("Results/Results23.RData")
load("Results/Results33.RData")
load("Results/Results43.RData")
load("Results/Results53.RData")
load("Results/Results83.RData")
load("Results/Combined.Rdata")
Results <- list(Results_23, Results_33, Results_43, Results_53, Results_83) #Combine results

## ---------------------------------------------------------------------------------------------------------------------------------
# Examining mean SE's per iteration
## ---------------------------------------------------------------------------------------------------------------------------------

# Compare results - Obtain statistics per iteration
CS <- lapply(Results, lapply, Class_stats)
CS <- list(do.call(rbind, CS[[1]]), do.call(rbind, CS[[2]]), do.call(rbind, CS[[3]]), do.call(rbind, CS[[4]]), do.call(rbind, CS[[5]]))

# Combine data
SE_long <- gdata::combine(CS[[1]][,6], CS[[2]][,6], CS[[3]][,6], CS[[4]][,6], CS[[5]][,6])
SE_wide <- as.data.frame(cbind(CS[[1]][,6], CS[[2]][,6], CS[[3]][,6], CS[[4]][,6], CS[[5]][,6])) 
names(SE_wide)<- c("m2", "m3", "m4", "m5", "m8")

# check for normality -- normal for subgroups
hist(SE_long$data)
hist(SE_wide$m2)
hist(SE_wide$m3)
hist(SE_wide$m4)
hist(SE_wide$m5)
hist(SE_wide$m8)

car::qqPlot(SE_long$data)
car::qqPlot(SE_wide$m2)
car::qqPlot(SE_wide$m3)
car::qqPlot(SE_wide$m4)
car::qqPlot(SE_wide$m5)
car::qqPlot(SE_wide$m8)

# check outliers
boxplot(SE_long$data)
boxplot(SE_wide)

# homogeneity of variance 
apply(CS[[1]], 2, var)[6]
apply(CS[[2]], 2, var)[6]
apply(CS[[3]], 2, var)[6]
apply(CS[[4]], 2, var)[6]
apply(CS[[5]], 2, var)[6]
#ratio's not bigger than 1:10

## ---------------------------------------------------------------------------------------------------------------------------------
# Examining accuracy measures for track placement advices
## ---------------------------------------------------------------------------------------------------------------------------------
# Combine data Cramer's V
CV_long <- gdata::combine(CS[[1]][,2], CS[[2]][,2], CS[[3]][,2], CS[[4]][,2], CS[[5]][,2])
CV_wide <-  as.data.frame(cbind(CS[[1]][,2], CS[[2]][,2], CS[[3]][,2], CS[[4]][,2], CS[[5]][,2])) 
names(CV_wide) <- c("m2", "m3", "m4", "m5", "m8")

# Combine data Kruskall-Goodman gamma
gamma_long <- gdata::combine(CS[[1]][,4], CS[[2]][,4], CS[[3]][,4], CS[[4]][,4], CS[[5]][,4])
gamma_wide <- as.data.frame(cbind(CS[[1]][,4], CS[[2]][,4], CS[[3]][,4], CS[[4]][,4], CS[[5]][,4])) 
names(gamma_wide) <- c("m2", "m3", "m4", "m5", "m8")

# Combine above two
data_combi <- cbind(CV_long, gamma_long)
data_combi <- data_combi[,c(1,3,4)]
names(data_combi) <- c("CV", "gamma", "bron")

# check for normality
hist(CV_long$data)
hist(CV_wide$m2)
hist(CV_wide$m3)
hist(CV_wide$m4)
hist(CV_wide$m5)
hist(CV_wide$m8)

hist(gamma_long$data)
hist(gamma_wide$m2)
hist(gamma_wide$m3)
hist(gamma_wide$m4)
hist(gamma_wide$m5)
hist(gamma_wide$m8)

## add qqplots
car::qqPlot(CV_long$data)
car::qqPlot(CV_wide$m2)
car::qqPlot(CV_wide$m3)
car::qqPlot(CV_wide$m4)
car::qqPlot(CV_wide$m5)
car::qqPlot(CV_wide$m8)

car::qqPlot(gamma_long$data)
car::qqPlot(gamma_wide)
car::qqPlot(gamma_wide$m3)
car::qqPlot(gamma_wide$m4)
car::qqPlot(gamma_wide$m5)
car::qqPlot(gamma_wide$m8)

# check outliers - some present, not too severe
boxplot(CV_wide) 
boxplot(gamma_wide)

# check outliers (manova)
psych::outlier(CV_wide)
which(mahalanobis_distance(CV_wide)[,5]==TRUE)

psych::outlier(gamma_wide)
which(rstatix::mahalanobis_distance(gamma_wide)[,5]==TRUE)

# homogeneity of variance 
heplots::boxM(data_combi[,1:2], group = data_combi$bron) #significant, however equal groups present

apply(CS[[1]], 2, var)[6]
apply(CS[[2]], 2, var)[6]
apply(CS[[3]], 2, var)[6]
apply(CS[[4]], 2, var)[6]
apply(CS[[5]], 2, var)[6]
# ratio's not bigger than 1:10

# multivariate normality tussen CV, GKG 
rstatix::mshapiro_test(data_combi[,1:2]) 

# linear relation between DV (CV, GKG) and (for?) each IV (m) group 
plot(CV_wide$m2, gamma_wide$m2)
plot(CV_wide$m3, gamma_wide$m3)
plot(CV_wide$m4, gamma_wide$m4)
plot(CV_wide$m5, gamma_wide$m5)
plot(CV_wide$m8, gamma_wide$m8)

# multicolinearity (cor between IV not >.9)
cor(cbind(data_combi$CV, data_combi$gamma)) 

# homoscedasticity and linearity (residusals ~ predicted values plot, plot after running analysis)
mod <- lm(cbind(data_combi$CV, data_combi$gamma) ~ data_combi$bron)
res <- mod$residuals
fit <- mod$fitted.values
plot(fit[,1],res[,1])
plot(fit[,2],res[,2])

