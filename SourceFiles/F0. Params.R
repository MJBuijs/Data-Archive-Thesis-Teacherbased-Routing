######
# F0. Parameters
#####

library(readxl)
library(gdata)
library(tidyverse)
library(mstR)
library(rcompanion) 
library(DescTools)
library(parallel)
library(doParallel)
library(foreach)
library(car)
library(ggplot2)
library(emmeans)
library(lattice)
library(reshape2)
library(rstatix)


# set parameters
n <- 10000 #number of students in each dataset
iter <- 1000 #number of datasets
mu <- 0 #mean (true) ability
sd <- 1 #sd (true) ability


grens_23 <- c(6)
grens_33 <- c(3, 6)
grens_43 <- c(3, 5, 7)
grens_53 <- c(2, 3, 5, 7) 
grens_83 <- c(1, 2, 3, 5, 6, 7, 8)
