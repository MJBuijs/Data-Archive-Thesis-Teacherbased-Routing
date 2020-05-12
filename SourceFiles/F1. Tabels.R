######
# F1. Tresholds
#####

## ---------------------------------------------------------------------------------------------------------------------------------
# Read in table
tabel2 <- read_excel("Overig/tabel5.xlsx")
rownames(tabel2) <-
  c(
    "teacher_bb",
    "teacher_bb/kb",
    "teacher_kb",
    "teacher_gt",
    "teacher_gt/havo",
    "teacher_havo",
    "teacher_havo/vwo",
    "teacher_vwo"
  )
colnames(tabel2) <-
  c(
    "EPST_bb",
    "EPST_bb/kb",
    "EPST_kb",
    "EPST_gt",
    "EPST_gt/havo",
    "EPST_havo",
    "EPST_havo/vwo",
    "EPST_vwo"
  )

# Conditional probabilities
cond.prob.bb <- tabel2[1:8,1]/sum(tabel2[1:8,1])
cond.prob.bb_kb <- tabel2[1:8,2]/sum(tabel2[1:8,2])
cond.prob.kb <- tabel2[1:8,3]/sum(tabel2[1:8,3])
cond.prob.gt <- tabel2[1:8,4]/sum(tabel2[1:8,4])
cond.prob.gt_havo <- tabel2[1:8,5]/sum(tabel2[1:8,5])
cond.prob.havo <- tabel2[1:8,6]/sum(tabel2[1:8,6])
cond.prob.havo_vwo <- tabel2[1:8,7]/sum(tabel2[1:8,7])
cond.prob.vwo <- tabel2[1:8,8]/sum(tabel2[1:8,8])

# Calculate percentages of advices
(
  CBS_data <- tabel2 %>%
    rownames_to_column() %>%
    gather(rowname2, value,-rowname) %>%
    rowwise() %>%
    mutate(value = list(1:value)) %>%
    unnest(value) %>%
    dplyr::select(-value) %>%
    rename(teacher_advice = rowname,
           EPST_advice = rowname2)

)
# tabelx and tabelxdoc; used in F3
# obtain: how often (percentage) each EPST and teacher's recommendation occurs in the CBS data
  (tabelx <- CBS_data %>%
    group_by(EPST_advice) %>%
    summarise(n = n()) %>%
    add_row(EPST_advice = "EPST_kb/gt", n = 0) %>%
    arrange(factor(
      EPST_advice,
      levels = c(
        "EPST_bb",
        "EPST_bb/kb",
        "EPST_kb",
        "EPST_kb/gt",
        "EPST_gt",
        "EPST_gt/havo",
        "EPST_havo",
        "EPST_havo/vwo",
        "EPST_vwo"
      )
    )) %>%
    mutate(percentage = n / 119754 * 100)
)
#
(tabelxdoc <- CBS_data %>%
    group_by(teacher_advice) %>%
    summarise(n = n()) %>%
    add_row(teacher_advice = "teacher_kb/gt", n=0) %>%
    arrange(factor(
      teacher_advice,
      levels = c(
      "teacher_bb",
      "teacher_bb/kb",
      "teacher_kb",
      "teacher_kb/gt",
      "teacher_gt",
      "teacher_gt/havo",
      "teacher_havo",
      "teacher_havo/vwo",
      "teacher_vwo"
    ))) %>%
    mutate(percentage = n / 119754 * 100)
)
rm(CBS_data)
# CBS_data <- CBS_data %>% 
#   mutate(EPST_advice = dplyr::recode(EPST_advice, 
#                               "EPST_bb"=1,
#                               "EPST_bb/kb"=2,
#                               "EPST_kb"=3,
#                               "EPST_gt"=4,
#                               "EPST_gt/havo"=5,
#                               "EPST_havo"=6,
#                               "EPST_havo/vwo"=7,
#                               "EPST_vwo"=8),
#          teacher_advice = dplyr::recode(teacher_advice,
#                                  "teacher_bb"=1,
#                                  "teacher_bb/kb"=2,
#                                  "teacher_kb"=3,
#                                  "teacher_gt"=4,
#                                  "teacher_gt/havo"=5,
#                                  "teacher_havo"=6,
#                                  "teacher_havo/vwo"=7,
#                                  "teacher_vwo"=8),
#          dif = EPST_advice - teacher_advice,
#          abs = abs(dif))
# tabel1 <- table(CBS_data$abs)
# percentage <- round(tabel1/sum(tabel1) * 100, 4)
# CBS_data <- CBS_data %>% 
#   dplyr::select(-abs)
# 

