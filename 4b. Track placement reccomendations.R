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
Results <- list(Results_23, Results_33, Results_43, Results_53, Results_83) 
rm(Results_23, Results_33, Results_43, Results_53, Results_83)

## ---------------------------------------------------------------------------------------------------------------------------------
##### Testing differences
## ---------------------------------------------------------------------------------------------------------------------------------

# obtain statistics
CS <- lapply(Results, lapply, Class_stats)
CS <- list(do.call(rbind, CS[[1]]), do.call(rbind, CS[[2]]), do.call(rbind, CS[[3]]), do.call(rbind, CS[[4]]), do.call(rbind, CS[[5]]))

# combine statistics into dataframes
CV_long <- gdata::combine(CS[[1]][,2], CS[[2]][,2], CS[[3]][,2], CS[[4]][,2], CS[[5]][,2])
CV_wide <-  as.data.frame(cbind(CS[[1]][,2], CS[[2]][,2], CS[[3]][,2], CS[[4]][,2], CS[[5]][,2])) 
names(CV_wide) <- c("m2", "m3", "m4", "m5", "m8")

gamma_long <- gdata::combine(CS[[1]][,4], CS[[2]][,4], CS[[3]][,4], CS[[4]][,4], CS[[5]][,4])
gamma_wide <- as.data.frame(cbind(CS[[1]][,4], CS[[2]][,4], CS[[3]][,4], CS[[4]][,4], CS[[5]][,4])) 
names(gamma_wide) <- c("m2", "m3", "m4", "m5", "m8")

data_combi <- cbind(CV_long, gamma_long)
data_combi <- data_combi[,c(1,3,4)]
names(data_combi) <- c("CV", "gamma", "bron")

rm(CS)

# Manova over CV, gamma 
man <- manova(cbind(data_combi$CV, data_combi$gamma)~data_combi$bron)
summary(man)
summary.aov(man)
pairwise.t.test(data_combi$CV, data_combi$bron, "bonferroni")
pairwise.t.test(data_combi$gamma, data_combi$bron, "bonferroni")

aov1 <- aov(data_combi$CV ~ data_combi$bron)
emmeans(aov1, ~ bron) #table 2
aov2 <- aov(data_combi$gamma ~ data_combi$bron)
emmeans(aov2, ~ bron) #table 2

## ---------------------------------------------------------------------------------------------------------------------------------
##### Tables
## ---------------------------------------------------------------------------------------------------------------------------------
load("Results/Combined.RData")
tab1 <- lapply(list(Combined_23, Combined_33, Combined_43, Combined_53, Combined_83), Overeenkomst_tabel, type = "true")
tabEPST <- Overeenkomst_tabel(Combined_23, type = "EPST")
tab <- list(tab1[[1]], tab1[[2]], tab1[[3]], tab1[[4]], tab1[[5]], tabEPST)

# save(tab, file = "Results/tabs_true.RData")
# load("Results/tabs_true.RData")

# Figures 7A-E
lapply(tab1, heatmapfunction, xlab = "optimal track placement", ylab = "Teacher's Recommendation", min = .3, marg = FALSE)

# Figures 6
track <- cbind(tab[[1]][,9], tab[[2]][,9], tab[[3]][,9], tab[[4]][,9], tab[[5]][,9], tab[[6]][,9])
rownames(track) <- c("bb", "bb_kb", "kb", "gt", "gt_havo", "havo", "havo_vwo", "vwo", "total")
colnames(track) <- c("2 modules", "3 modules", "4 modules", "5 modules", "8 modules", "Regular routing")
docent <- rbind(tab[[1]][9,], tab[[2]][9,], tab[[3]][9,], tab[[4]][9,], tab[[5]][9,], tab[[6]][9,])
rownames(docent) <- c("2 modules", "3 modules", "4 modules", "5 modules", "8 modules", "Regular routing")
colnames(docent) <- c("bb", "bb_kb", "kb", "gt", "gt_havo", "havo", "havo_vwo", "vwo", "total")
heatmapfunction(track, "number of modules in the first stage", "Teacher's recommendation", min = .65, marg = TRUE)
heatmapfunction(t(docent), "number of modules in the first stage", "Optimal track placement", min = .65, marg=TRUE)

# combine data (decrease size)
Combined_23 <- Combined_23 %>% 
  select(plaatsing, toets_advies, design_advies, mod)
Combined_33 <- Combined_33 %>% 
  select(plaatsing, design_advies, mod, toets_advies)
Combined_43 <- Combined_43 %>% 
  select(plaatsing, design_advies, mod, toets_advies)
Combined_53 <- Combined_53 %>% 
  select(plaatsing, design_advies, mod, toets_advies)
Combined_83 <- Combined_83 %>% 
  select(plaatsing, design_advies, mod, toets_advies)
Combined <- rbind(Combined_23, Combined_33, Combined_43, Combined_53, Combined_83)

# Add categories
Combined1 <- Combined %>%
  mutate(
    toets_advies = as.numeric(as.character(toets_advies)),
    design_advies = as.numeric(as.character(design_advies)),
    overeenkomst = if_else
    ((
      toets_advies == design_advies |
        toets_advies == design_advies + 1 |
        toets_advies == design_advies - 1
    )
      ,
      ("0-0.5"),
      if_else(
        (toets_advies == design_advies + 2 |
           toets_advies == design_advies - 2 |
           toets_advies == design_advies + 3 |
           toets_advies == design_advies - 3 |
           toets_advies == design_advies + 4 |
           toets_advies == design_advies - 4 ),
        "1 - 2 levels difference",
        if_else(
          (toets_advies == design_advies - 6 |
             toets_advies == design_advies + 6 |
             toets_advies == design_advies + 7 |
             toets_advies == design_advies - 7 |
             toets_advies == design_advies - 8 |
             toets_advies == design_advies + 8 |
             toets_advies == design_advies + 5 |
             toets_advies == design_advies - 5),"2.5 or more levels difference","100"
        )
      ))) %>%
  select(-c(toets_advies, design_advies)
  )

# obtain Table 3
tabel1 <- table(Combined1$overeenkomst, Combined1$mod)

#print percentages
print(tabel1/100000)
