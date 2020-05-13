rm(list = ls())
set.seed(42)

# source files containing the functions, CBS table etc. 
file.sources <- list.files(path = "SourceFiles", pattern="*.R", full.names=TRUE)
sapply(file.sources, source, .GlobalEnv)

# load results
load("Results/Results23.RData")
load("Results/Results33.RData")
load("Results/Results43.RData")
load("Results/Results53.RData")
load("Results/Results83.RData")
load("Results/Combined.RData")
Results <- list(Results_23, Results_33, Results_43, Results_53, Results_83) 

# obtain statistics
CS <- lapply(Results, lapply, Class_stats)
CS <- list(do.call(rbind, CS[[1]]), do.call(rbind, CS[[2]]), do.call(rbind, CS[[3]]), do.call(rbind, CS[[4]]), do.call(rbind, CS[[5]]))

rm(Results_23, Results_33, Results_43, Results_53, Results_83, Results)

## ---------------------------------------------------------------------------------------------------------------------------------
##### Testing differences between mean SE
## ---------------------------------------------------------------------------------------------------------------------------------
# combine data
SE_long <- gdata::combine(CS[[1]][,7], CS[[1]][,6], CS[[2]][,6], CS[[3]][,6], CS[[4]][,6], CS[[5]][,6])
SE_long <- SE_long %>% 
  mutate(source = dplyr::recode(source, 
                                `CS[[1]][, 7]` = "RR",
                                `CS[[1]][, 6]` = "m2",
                                `CS[[2]][, 6]` = "m3",
                                `CS[[3]][, 6]` = "m4",
                                `CS[[4]][, 6]` = "m5",
                                `CS[[5]][, 6]` = "m8")) 

# combine data
SE_wide <- as.data.frame(cbind(CS[[1]][,7], CS[[1]][,6], CS[[2]][,6], CS[[3]][,6], CS[[4]][,6], CS[[5]][,6])) 
names(SE_wide)<- c("RR", "m2", "m3", "m4", "m5", "m8")

# run ANOVA
anov <- (aov(SE_long$data ~ SE_long$source))
summary(anov)
pairwise.t.test(SE_long$data, SE_long$source, "bonferroni")
emmeans(anov, ~ source) #Table 1
rm(SE_long, SE_wide, CS)

## ---------------------------------------------------------------------------------------------------------------------------------
##### SE plots
## ---------------------------------------------------------------------------------------------------------------------------------
# Add panel that each student took to Combined datasets
Combined_23 <- Combined_23 %>% 
  select(SE_test, theta_estimate, dif, mod, startmod, secmod, true_dif) %>% 
  unite("panel", startmod:secmod, sep = "-", remove = FALSE) 

Combined_33 <- Combined_33 %>% 
  select(SE_test, theta_estimate, dif, mod, startmod, secmod, true_dif) %>% 
  unite("panel", startmod:secmod, sep = "-", remove = FALSE)

Combined_43 <- Combined_43 %>% 
  select(SE_test, theta_estimate, dif, mod, startmod, secmod, true_dif) %>% 
  unite("panel", startmod:secmod, sep = "-", remove = FALSE) 

Combined_53 <- Combined_53 %>% 
  select(SE_test, theta_estimate, dif, mod, startmod, secmod, true_dif) %>% 
  unite("panel", startmod:secmod, sep = "-", remove = FALSE) 

## ------------------------------------------------------
# Round SE's and ability estimates and only keep the unique cases (to decrease size of dataset so fewer memory errors occur)
# Due to size of objects; first round values and decrease number of variables, then filter for unique rows
Combined_23 <- Combined_23%>% 
  mutate(SE_test = round(SE_test, 3),
         theta_estimate = round(theta_estimate,3)) %>% 
  mutate(difference = if_else((dif == 0 | dif == 1),"0-.5 level", if_else
                              ((dif == 2 | dif == 3 | dif == 4 | dif == 5), "1-2.5 levels", "3+ levels"
                              )),
         true_diff = if_else((true_dif == 0 | true_dif == 1),"0-.5 level", if_else
                             ((true_dif == 2 | true_dif == 3 | true_dif == 4 | true_dif == 5), "1-2.5 levels", "3+ levels"
                             )))  %>% 
  select(-(c(dif, true_dif)))

Combined_33 <- Combined_33%>% 
  mutate(SE_test = round(SE_test, 3),
         theta_estimate = round(theta_estimate,3))%>% 
  mutate(difference = if_else((dif == 0 | dif == 1),"0-.5 level", if_else
                              ((dif == 2 | dif == 3 | dif == 4 | dif == 5), "1-2.5 levels", "3+ levels"
                              )),
         true_diff = if_else((true_dif == 0 | true_dif == 1),"0-.5 level", if_else
                             ((true_dif == 2 | true_dif == 3 | true_dif == 4 | true_dif == 5), "1-2.5 levels", "3+ levels"
                             )))%>% 
  select(-(c(dif, true_dif)))

Combined_43 <- Combined_43 %>% 
  mutate(SE_test = round(SE_test, 3),
         theta_estimate = round(theta_estimate,3))%>% 
  mutate(difference = if_else((dif == 0 | dif == 1),"0-.5 level", if_else
                              ((dif == 2 | dif == 3 | dif == 4 | dif == 5), "1-2.5 levels", "3+ levels"
                              )),
         true_diff = if_else((true_dif == 0 | true_dif == 1),"0-.5 level", if_else
                             ((true_dif == 2 | true_dif == 3 | true_dif == 4 | true_dif == 5), "1-2.5 levels", "3+ levels"
                             ))) %>% 
  select(-(c(dif, true_dif)))


Combined_53 <- Combined_53 %>% 
  mutate(SE_test = round(SE_test, 3),
         theta_estimate = round(theta_estimate,3)) %>% 
  mutate(difference = if_else((dif == 0 | dif == 1),"0-.5 level", if_else
                              ((dif == 2 | dif == 3 | dif == 4 | dif == 5), "1-2.5 levels", "3+ levels"
                              )),
         true_diff = if_else((true_dif == 0 | true_dif == 1),"0-.5 level", if_else
                             ((true_dif == 2 | true_dif == 3 | true_dif == 4 | true_dif == 5), "1-2.5 levels", "3+ levels"
                             ))) %>% 
  select(-(c(dif, true_dif)))

Combined_23 <- unique(Combined_23)%>% 
  mutate(mod = as.factor(mod),
         difference = as.factor(difference),
         true_diff = as.factor(true_diff))

Combined_33 <- Combined_33%>% 
  unique() %>% 
  mutate(mod = as.factor(mod),
         difference = as.factor(difference),
         true_diff = as.factor(true_diff))

Combined_43 <- Combined_43 %>% 
  unique() %>% 
  mutate(mod = as.factor(mod),
         difference = as.factor(difference),
         true_diff = as.factor(true_diff))

Combined_53 <- Combined_53 %>% 
  unique() %>% 
  mutate(mod = as.factor(mod),
         difference = as.factor(difference),
         true_diff = as.factor(true_diff))

EPST_SE <- Combined_83 %>%
  select(c(EPST_SE, EPST_est, dif, startmod, secmod, true_dif)) %>%
  mutate(mod = "Regular routing") %>%
  rename("SE_test" = "EPST_SE",
         "theta_estimate" = "EPST_est") %>% 
  unite("panel", startmod:secmod, sep = "-", remove = FALSE) %>% 
  mutate(SE_test = round(SE_test, 3),
         theta_estimate = round(theta_estimate,3),
         mod = "regular routing stage")%>% 
  mutate(difference = if_else((dif == 0 | dif == 1),"0-.5 level", if_else
                              ((dif == 2 | dif == 3 | dif == 4 | dif == 5), "1-2.5 levels", "3+ levels"
                              )),
         true_diff = if_else((true_dif == 0 | true_dif == 1),"0-.5 level", if_else
                             ((true_dif == 2 | true_dif == 3 | true_dif == 4 | true_dif == 5), "1-2.5 levels", "3+ levels"
                             )))%>% 
  unique() %>% 
  mutate(mod = as.factor(mod),
         dif = as.factor(dif),
         true_diff = as.factor(true_diff)) %>% 
  select(-c(dif, true_dif))

Combined_83 <- Combined_83 %>% 
  select(SE_test, theta_estimate, dif, mod, startmod, secmod, true_dif) %>% 
  unite("panel", startmod:secmod, sep = "-", remove = FALSE) 

Combined_83 <- Combined_83%>% 
  mutate(SE_test = round(SE_test, 3),
         theta_estimate = round(theta_estimate,3)) 
 
Combined_83 <- Combined_83%>% 
  mutate(difference = if_else((dif == 0 | dif == 1),"0-.5 level", if_else
                              ((dif == 2 | dif == 3 | dif == 4 | dif == 5), "1-2.5 levels", "3+ levels"
                              )),
         true_diff = if_else((true_dif == 0 | true_dif == 1),"0-.5 level", if_else
                             ((true_dif == 2 | true_dif == 3 | true_dif == 4 | true_dif == 5), "1-2.5 levels", "3+ levels"
                             )))%>% 
  select(-c(true_dif, dif))
Combined_83 <- Combined_83%>% 
  unique() %>% 
  mutate(mod = as.factor(mod),
         difference = as.factor(difference),
         true_diff = as.factor(true_diff))

## ------------------------------------------------------
# Save, empty cache, reload objects; prevents memory issues
save(Combined_23, Combined_33, Combined_43, Combined_53, Combined_83, EPST_SE, file = "Results/Combined_uniques.RData")
rm(list = ls())
load("Results/Combined_uniques.RData")

# Datsets where only the most easy, most intermediate and most difficulty combinations of modules exist + the most extreme mismatches (so most easy module to most difficult module and vice versa)
extremes_23 <- Combined_23 %>%
  mutate(panel = dplyr::recode(panel, "1-5" = "easy-difficult", "2-3" =  "difficult-easy", "1-3" = "easy-easy", "2-5"="difficult-difficult", "1-4"="easy-middle", "2-4"="difficult-middle"))
extremes_33 <- Combined_33 %>%
  filter(panel == "1-6" | panel == "3-4" | panel == "1-4" | panel == "3-6" | panel == "2-5") %>%
  mutate(panel = dplyr::recode(panel, "1-6" = "easy-difficult", "3-4" =  "difficult-easy", "1-4" = "easy-easy", "3-6"="difficult-difficult", "2-5" = "middle-middle"))
extremes_43 <- Combined_43 %>%
  filter(panel == "1-7" | panel == "4-5" | panel == "1-5" | panel == "4-7" | panel == "2-6" | panel == "3-6") %>%
  mutate(panel = dplyr::recode(panel, "1-7" = "easy-difficult", "4-5" =  "difficult-easy", "1-5" = "easy-easy", "4-7"="difficult-difficult", "2-6"="middle-middle", "3-6" = "middle-middle"))
extremes_53 <- Combined_53 %>%
  filter(panel == "1-8" | panel == "5-6" | panel == "1-6" | panel == "5-8" | panel == "3-7") %>%
  mutate(panel = dplyr::recode(panel, "1-8" = "easy-difficult", "5-6" =  "difficult-easy", "1-6" = "easy-easy", "5-8"="difficult-difficult", "3-7" = "middle-middle"))
extremes_83 <- Combined_83 %>%
  filter(panel == "1-11" | panel == "8-9" | panel == "1-9" | panel == "8-11" | panel == "4-10" | panel == "5-10") %>%
  mutate(panel = dplyr::recode(panel, "1-11" = "easy-difficult", "8-9" =  "difficult-easy", "1-9" = "easy-easy", "8-11"="difficult-difficult", "4-10" = "middle-middle", "5-10" = "middle-middle"))
plotextreme <- rbind(EPST_SE, extremes_23, extremes_33, extremes_43, extremes_53, extremes_83)

## ---------------------------------------------------------------------------------------------------------------------------------
#plot Figure 5
lattice::xyplot(
  SE_test ~ theta_estimate | true_diff ,
  data = plotextreme,
  group = mod,
  auto.key = TRUE,
  as.table = TRUE,
  strip = strip.custom(bg = "lightgrey", par.strip.text = list(col = "black")),
  layout = c(3,1),
  par.settings = list(superpose.symbol = list(pch = 19, cex = .5, col = c("red", "orange", "yellow", "green", "blue", "purple"))),
  xlab = "Ability estimate",
  ylab = "SEM"
)
