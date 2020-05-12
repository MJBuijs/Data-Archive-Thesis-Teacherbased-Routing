
# Packages laden
rm(list=ls())
.libPaths("M:/R/Lib3.5")
library(tidyverse)
library(haven)
library(foreign)

######## Inclusiecriterium 1: personen zijn opgenomen in het CitoTab bestand ####################

####### Inclusiecriterium 2: Personen zijn opgenomen in de gemeentelijke basisadministratie ######################## 

# inladen CITOTAB15
cito_1415 <- 
  haven::read_sav(file = "G:/Onderwijs/CITOTAB/2015/CITOTAB2015V1.SAV") %>% 
  select(RINPERSOONS, RINPERSOON, CitoAdviesLeerkracht, CitoStandaardScore, CitoSchooltypeadvies)
nrow(cito_1415) # personen in citotab 

# Personen filteren die zijn opgenomen in de GBA
cito_1415 <- cito_1415 %>% 
  filter(RINPERSOONS == "R") 
nrow(cito_1415) #personen in CitoTab die zijn ingeschreven in de GBA
cito_1415$RINPERSOON <- as.character(cito_1415$RINPERSOON)

# inladen INSCHRTAB voor het jaar 15/16 en filteren op GBA
onderwijs_1516 <- 
  haven::read_sav(file = "G:/Onderwijs/ONDERWIJSINSCHRTAB/2015/ONDERWIJSINSCHRTAB2015V2.SAV") %>% 
  filter(HOOFDINSCHR %in% c("01", "11"), RINPERSOONS == "R") %>% 
  # 01 en 11 zijn beide hoofdinschrijvingen VO (andere nummers is bijv. MBO, etc)
  select(RINPERSOONS, RINPERSOON, HOOFDINSCHR,TYPEONDERWIJS, OPLNR)
onderwijs_1516$RINPERSOON <- as.character(onderwijs_1516$RINPERSOON)
onderwijs_1516$OPLNR <- as.character(onderwijs_1516$OPLNR)

# inladen INSCHRTAB voor het jaar 17/18 en filteren op GBA
onderwijs_1718 <- 
  haven::read_sav(file = "G:/Onderwijs/ONDERWIJSINSCHRTAB/2017/ONDERWIJSINSCHRTAB2017V2.SAV") %>% 
  filter(HOOFDINSCHR %in% c("01", "11"), RINPERSOONS == "R") %>%  
  select(RINPERSOONS, RINPERSOON, HOOFDINSCHR, TYPEONDERWIJS, OPLNR)
onderwijs_1718$RINPERSOON <- as.character(onderwijs_1718$RINPERSOON)
onderwijs_1718$OPLNR <- as.character(onderwijs_1718$OPLNR)

# Data citotab combineren met onderwijs_1516 
cito_plaatsing <- cito_1415 %>% 
  left_join(onderwijs_1516, by = "RINPERSOON") %>% 
  rename(HOOFDINSCHR_1516 = HOOFDINSCHR,
         TYPEONDERWIJS_1516 = TYPEONDERWIJS,
         OPLNR_1516 = OPLNR)

# En data combineren met onderwijs_1718
cito_plaatsing <- cito_plaatsing %>% 
  left_join(onderwijs_1718, by = "RINPERSOON") %>% 
  select(-RINPERSOONS) %>% 
  rename(HOOFDINSCHR_1718 = HOOFDINSCHR,
         TYPEONDERWIJS_1718 = TYPEONDERWIJS,
         OPLNR_1718 = OPLNR)

####### Inclusiecriterium 3: Er moet ook een docentadvies bekend zijn ######################## 

# Recoderen van variabele CitoSchooltypeadvies en schooltype_label_1718 + studenten selecteren die óók een docentadvies hebben.
cito_plaatsing$CitoAdviesLeerkracht<-as.factor(cito_plaatsing$CitoAdviesLeerkracht)
cito_plaatsing <-
  cito_plaatsing %>% 
  filter(!CitoAdviesLeerkracht==99) %>%   #selecteer alleen studenten die ook een docentadvies hebben
  mutate(CitoAdviesLeerkracht = recode(CitoAdviesLeerkracht, 
                                       "01"="0", #bb
                                       "02"="0.5", #bb_kb
                                       "03"="1", #kb
                                       "04"="1.5", #kb_gt
                                       "05"="2", #gt
                                       "06"="2.5", #gt_havo
                                       "07"="77", #gt_havo_vwo < breed
                                       "08"="3", #havo
                                       "09"="3.5", #havo_vwo
                                       "10"="4"))#vwo
nrow(cito_plaatsing) # studenten met een docent én toetsadvies

# Omcodering OPLNR-ILT-Advies 
# In de Onderwijsinschrtab is de variabele "OPLNR" opgenomen die we d.m.v. het bestand OPLNR_ILT_schooltype 
# (zie map H:\cito\OPLNR_ILT_schooltype) kunnen koppelen aan het juiste onderwijsnivea. 

# Inlezen van OPLNR_ilt_advies
oplnr_ilt_advies <-
  read_delim("H:/oplnr_ilt_advies_2018.csv",
             delim = ";",
             col_names = TRUE,
             col_types =
               cols(
                 ILT = col_character(),
                 schooltype_code = col_integer(),
                 schooltype_Cito = col_character(),
                 OPLNR = col_character(),
                 Omschrijving = col_character()
               )
  ) %>%
  filter(schooltype_code  %in% 1:13) %>% 
  mutate(schooltype_label =
           fct_recode(factor(schooltype_code),
                      "0" = "1", #bb
                      "0.5" = "2", #bb_kb
                      "1" = "3", #kb
                      "1.5" = "4", #kb_gt
                      "2" = "5", #gt
                      "2.5" = "6", #gt_havo
                      "77" = "7", #gt_havo_vwo <breed
                      "3" = "8", #gt
                      "3.5" = "9", #havo_vwo
                      "4" = "10",#vwo
                      "77" = "11", #bb_kb_gt
                      "77" = "12", #bb_kb_gt_havo_vwo
                      "13" = "13") #pro
  )

# Koppelen oplnr_ilt_advies aan data
cito_plaatsing <-
  cito_plaatsing %>% 
  left_join(y = unique(oplnr_ilt_advies[, c("OPLNR", "schooltype_code", "schooltype_label")]),
            by = c("OPLNR_1516"= "OPLNR")) %>% 
  rename(schooltype_code_1516 = schooltype_code, 
         schooltype_label_1516 = schooltype_label)%>% 
  left_join(y = unique(oplnr_ilt_advies[, c("OPLNR", "schooltype_code", "schooltype_label")]),
            by = c("OPLNR_1718"= "OPLNR")) %>% 
  rename(schooltype_code_1718 = schooltype_code, 
         schooltype_label_1718 = schooltype_label) 

####### Inclusiecriterium 4: Plaatsing VO in 15/16 en 17/18 moet bekend zijn ######################## 

# aantal leerlingen waarbij plaatsing ontbreekt 
nrow(cito_plaatsing[is.na(cito_plaatsing$schooltype_label_1516)&!is.na(cito_plaatsing$schooltype_label_1718),]) 
nrow(cito_plaatsing[is.na(cito_plaatsing$schooltype_label_1718)&!is.na(cito_plaatsing$schooltype_label_1516),]) 
nrow(cito_plaatsing[is.na(cito_plaatsing$schooltype_label_1718)&is.na(cito_plaatsing$schooltype_label_1516),]) 
nrow(cito_plaatsing[!is.na(cito_plaatsing$schooltype_label_1718)&!is.na(cito_plaatsing$schooltype_label_1516),])

# verwijder personen waarvoor de plaatsing onbekend is
cito_plaatsing <- cito_plaatsing[!is.na(cito_plaatsing$schooltype_code_1516) & !is.na(cito_plaatsing$schooltype_code_1718),]
nrow(cito_plaatsing) #personen waarvan de plaatsing bekend

####### Inclusiecriterium 5: Plaatsing is niet in het praktijkonderwijs ######################## 

# verwijder wanneer praktijkonderwijs wordt gevolgd
cito_plaatsing <- cito_plaatsing[cito_plaatsing$schooltype_label_1516 != "13" & cito_plaatsing$schooltype_label_1718 != "13",]
nrow(cito_plaatsing) #personen waarvan ook bekend is dat ze geen praktijkonderwijs volgen

# ----------------------------------------------------------------------------------------------------------------------------------
# In CitoTab is een variabele met toetsadvies opgenomen. Vergelijken met het berekende advies a.d.h.v. standaardscore:

# Omzetting standaardscores van Cito naar niveau-advies 
SS_advies_ozt <-
  tribble(
    ~adviescode, ~advieslabel, ~SS_min, ~SS_max,
    0,       "bb", 501, 518,
    0.5,    "bb_kb", 519, 525,
    1,       "kb", 526, 528,
    2,       "gt", 529, 532,
    2.5,  "gt_havo", 533, 536,
    3,     "havo", 537, 539,
    3.5, "havo_vwo", 540, 544,
    4,      "vwo", 545, 550
  )
SS_cut <- c(501, 519, 526, 529, 533, 537, 540, 545, 550)

# toevoegen CETadvies aan bestand "cito_plaatsing"
cito_plaatsing <-
  cito_plaatsing %>% 
  mutate(cetadvies = cut(CitoStandaardScore,
                         breaks = SS_cut,
                         labels = SS_advies_ozt$adviescode,
                         include.lowest = TRUE,
                         right = FALSE))

################################################################################################## 

# selecteer goede variabelen
dat <- cito_plaatsing %>% 
  select(CitoAdviesLeerkracht, cetadvies, schooltype_label_1718)

summary(dat$CitoAdviesLeerkracht) 
summary(dat$cetadvies) 
summary(dat$schooltype_label_1718)

#maak numerieke values
dat$CitoAdviesLeerkracht <- as.numeric(as.character(dat$CitoAdviesLeerkracht))
dat$cetadvies <- as.numeric(as.character(dat$cetadvies))
dat$schooltype_label_1718 <- as.numeric(as.character(dat$schooltype_label_1718))

# double check getallen
table(dat$CitoAdviesLeerkracht)
table(dat$cetadvies)
table(dat$schooltype_label_1718) 

#afstanden tussen adviezen berekenen
dat <- dat %>% 
  mutate(afstand_cet_van_docent = CitoAdviesLeerkracht - cetadvies,
         afstand_VO3_van_docent = CitoAdviesLeerkracht - schooltype_label_1718,
         afstand_docent_van_cet = cetadvies - CitoAdviesLeerkracht,
         afstand_VO3_van_cet = cetadvies - schooltype_label_1718)  %>% 
    filter(afstand_cet_van_docent >= 3 | afstand_cet_van_docent <= -3)

dat$afstand_cet_van_docent <- as.factor(dat$afstand_cet_van_docent)
dat$afstand_docent_van_cet <- as.factor(dat$afstand_docent_van_cet)

#categorieën omcoderen
dat <- dat %>% 
  mutate(afstand_docent_van_cet = recode(afstand_docent_van_cet,
                                         "-4"= "meer dan 3 lager",
                                         "-3.5"= "meer dan 3 lager",
                                         "-3" = "meer dan 3 lager",
                                         "3" = "meer dan 3 hoger",
                                         "3.5" = "meer dan 3 hoger",
                                         "4" = "meer dan 3 hoger"),
         afstand_cet_van_docent = recode(afstand_cet_van_docent,
                                         "-4"= "meer dan 3 lager",
                                         "-3.5"= "meer dan 3 lager",
                                         "-3" = "meer dan 3 lager",
                                         "3" = "meer dan 3 hoger",
                                         "3.5" = "meer dan 3 hoger",
                                         "4" = "meer dan 3 hoger"),
         afstand_VO3_van_cet = recode(afstand_VO3_van_cet,
                                      "-77" = "brede stroom",
                                      "-4.0"= "meer dan 3 lager",
                                      "-3.5"= "meer dan 3 lager",
                                      "-3.0" = "meer dan 3 lager",
                                      "-2.5" = "2-2.5 lager",
                                      "-2.0" = "2-2.5 lager",
                                      "-1.5" = "1-1.5 lager",
                                      "-1.0" = "1-1.5 lager",
                                      "-0.5" = "0 of .5 verschil",
                                      "0.0"= "0 of .5 verschil",
                                      "0.5" = "0 of .5 verschil",
                                      "1.0" = "1-1.5 hoger",
                                      "1.5"= "1-1.5 hoger",
                                      "2.0"="2-2.5 hoger",
                                      "2.5"="2-2.5 hoger",
                                      "3" = "meer dan 3 hoger",
                                      "3.5" = "meer dan 3 hoger",
                                      "4" = "meer dan 3 hoger") ,
         afstand_VO3_van_docent = recode(afstand_VO3_van_docent, 
                                         "-74" = "brede stroom",
                                         "-4.0"= "meer dan 3 lager",
                                         "-3.5"= "meer dan 3 lager",
                                         "-3.0" = "meer dan 3 lager",
                                         "-2.5" = "2-2.5 lager",
                                         "-2.0" = "2-2.5 lager",
                                         "-1.5" = "1-1.5 lager",
                                         "-1.0" = "1-1.5 lager",
                                         "-0.5" = "0 of .5 verschil",
                                         "0.0"= "0 of .5 verschil",
                                         "0.5" = "0 of .5 verschil",
                                         "1.0" = "1-1.5 hoger",
                                         "1.5"= "1-1.5 hoger",
                                         "2.0"="2-2.5 hoger",
                                         "2.5"="2-2.5 hoger",
                                         "3" = "meer dan 3 hoger",
                                         "3.5" = "meer dan 3 hoger",
                                         "4" = "meer dan 3 hoger")
  )

# tabellen maken
(tabel1 <- dat %>% 
  group_by(afstand_cet_van_docent, afstand_VO3_van_docent) %>% 
    tally())
tabel1$n[tabel1$n<10]<- "<10"

(tabel2 <- dat %>% 
    group_by(afstand_docent_van_cet, afstand_VO3_van_cet) %>% 
    tally())
tabel2$n[tabel2$n<10]<- "<10"

# tabellen opslaan 
XLConnect::writeWorksheetToFile("project_8482_tabel_10.xlsx", tabel1, sheet="1")
XLConnect::writeWorksheetToFile("project_8482_tabel_10.xlsx", tabel2, sheet="2")
