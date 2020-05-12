
# Packages laden
rm(list=ls())
.libPaths("M:/R/Lib3.5")
library(tidyverse)
library(haven)
library(foreign)
library(DescTools)

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
cito_1415$CitoAdviesLeerkracht <- as.factor(cito_1415$CitoAdviesLeerkracht)
cito_1415$CitoSchooltypeadvies <- as.factor(cito_1415$CitoSchooltypeadvies)
cito_1415$RINPERSOON <- as.character(cito_1415$RINPERSOON)

# inladen INSCHRTAB voor het jaar 15/16 en filteren op GBA
onderwijs_1516 <- 
  haven::read_sav(file = "G:/Onderwijs/ONDERWIJSINSCHRTAB/2015/ONDERWIJSINSCHRTAB2015V2.SAV") %>% 
  filter(HOOFDINSCHR %in% c("01", "11"), RINPERSOONS == "R") %>% 
  # 01 en 11 zijn beide hoofdinschrijvingen VO (andere nummers is bijv. MBO, etc)
  select(RINPERSOONS, RINPERSOON, HOOFDINSCHR,TYPEONDERWIJS, OPLNR)
onderwijs_1516$HOOFDINSCHR <- as.factor(onderwijs_1516$HOOFDINSCHR)
onderwijs_1516$TYPEONDERWIJS <- as.factor(onderwijs_1516$TYPEONDERWIJS)
onderwijs_1516$RINPERSOON <- as.character(onderwijs_1516$RINPERSOON)
onderwijs_1516$OPLNR <- as.character(onderwijs_1516$OPLNR)

# inladen INSCHRTAB voor het jaar 17/18 en filteren op GBA
onderwijs_1718 <- 
  haven::read_sav(file = "G:/Onderwijs/ONDERWIJSINSCHRTAB/2017/ONDERWIJSINSCHRTAB2017V2.SAV") %>% 
  filter(HOOFDINSCHR %in% c("01", "11"), RINPERSOONS == "R") %>%  
  select(RINPERSOONS, RINPERSOON, HOOFDINSCHR, TYPEONDERWIJS, OPLNR)
onderwijs_1718$HOOFDINSCHR <- as.factor(onderwijs_1718$HOOFDINSCHR)
onderwijs_1718$TYPEONDERWIJS <- as.factor(onderwijs_1718$TYPEONDERWIJS)
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
cito_plaatsing <-
  cito_plaatsing %>% 
  filter(!CitoAdviesLeerkracht==99) %>% #selecteer alleen studenten die ook een docentadvies hebben
  mutate(CitoAdviesLeerkracht = recode(CitoAdviesLeerkracht, 
                                       "01"="bb",
                                       "02"="bb_kb",
                                       "03"="kb",
                                       "04"="kb_gt",
                                       "05"="gt",
                                       "06"="gt_havo",
                                       "07"="gt_havo_vwo",
                                       "08"="havo",
                                       "09"="havo_vwo",
                                       "10"="vwo")) %>% 
  mutate(CitoSchooltypeadvies = recode(CitoSchooltypeadvies, 
                                       "0"= "Pro",
                                       "1"="bb",
                                       "2"="kb",
                                       "3"="gt",
                                       "4"="havo",
                                       "5"="vwo",
                                       "6"="bb_kb",
                                       "7"="gt_havo",
                                       "8"="havo_vwo")) 
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
                      "bb" = "1",
                      "bb_kb" = "2",
                      "kb" = "3",
                      "kb_gt" = "4",
                      "gt" = "5",
                      "gt_havo" = "6",
                      "gt_havo_vwo" = "7",
                      "havo" = "8",
                      "havo_vwo" = "9",
                      "vwo" = "10",
                      "bb_kb_gt" = "11",
                      "bb_kb_gt_havo_vwo" = "12",
                      "pro" = "13")
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

cito_plaatsing$schooltype_label_1516 <- as.factor(cito_plaatsing$schooltype_label_1516)
cito_plaatsing$schooltype_label_1718 <- as.factor(cito_plaatsing$schooltype_label_1718)


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
cito_plaatsing <- cito_plaatsing[cito_plaatsing$schooltype_label_1516 != "pro" & cito_plaatsing$schooltype_label_1718 != "pro",]
nrow(cito_plaatsing) #personen waarvan ook bekend is dat ze geen praktijkonderwijs volgen

# ----------------------------------------------------------------------------------------------------------------------------------
# In CitoTab is een variabele met toetsadvies opgenomen. Vergelijken met het berekende advies a.d.h.v. standaardscore:

# Omzetting standaardscores van Cito naar niveau-advies 
SS_advies_ozt <-
  tribble(
    ~adviescode, ~advieslabel, ~SS_min, ~SS_max,
    1,       "bb", 501, 518,
    2,    "bb_kb", 519, 525,
    3,       "kb", 526, 528,
    5,       "gt", 529, 532,
    6,  "gt_havo", 533, 536,
    8,     "havo", 537, 539,
    9, "havo_vwo", 540, 544,
    10,      "vwo", 545, 550
  )
SS_cut <- c(501, 519, 526, 529, 533, 537, 540, 545, 550)

# toevoegen CETadvies aan bestand "cito_plaatsing"
cito_plaatsing <-
  cito_plaatsing %>% 
  mutate(cetadvies = cut(CitoStandaardScore,
                         breaks = SS_cut,
                         labels = SS_advies_ozt$advieslabel,
                         include.lowest = TRUE,
                         right = FALSE))

# tabel van zelfberekende citoscore en bestaande score
(tab1 <- table(cito_plaatsing$cetadvies,
               cito_plaatsing$CitoSchooltypeadvies))

## Het berekende advies bestaat uit dubbele ipv enkelvoudige adviezen. 
# Voor de tabellen maken we gebruik van de dubbele adviezen (variabele cetadvies)
# ----------------------------------------------------------------------------------------------------------------------------------


###########################################################################################################
# 
# onderstaande tabellen zijn een aanvulling op de eerder geëxporteerde tabellen
# "project_8482_tabel_1" tot en met "project_8482_tabel_4".
#
###########################################################################################################


# -----------------------------------------------------------------------------------------------------
# Tabel 5: Kruistabel toetsadvies en docentadvies
# -----------------------------------------------------------------------------------------------------
cito_plaatsing$CitoAdviesLeerkracht<-droplevels(cito_plaatsing$CitoAdviesLeerkracht)
tab1 <- table(cito_plaatsing$CitoAdviesLeerkracht,
              cito_plaatsing$cetadvies)

# inclusief marginalen
marg_docent <- margin.table(tab1, margin=1)
marg_toets <- margin.table(tab1, margin=2)

tab2 <- cbind(tab1, marg_docent)
marg_toets <- c(marg_toets, sum(marg_toets))
(kruistabel <- rbind(tab2, marg_toets))

XLConnect::writeWorksheetToFile("project_8482_tabel_5.xlsx", kruistabel, sheet="1")

