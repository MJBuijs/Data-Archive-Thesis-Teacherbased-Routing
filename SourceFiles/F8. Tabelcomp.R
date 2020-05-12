######
# F8. Overeenkomst_tabel
#####

## ---------------------------------------------------------------------------------------------------------------------------------
# tables that provide the proportion of overlap between recommendations 

Overeenkomst_tabel <- function(data, type = c("true", "EPST", "n")){
  prop  <- matrix(ncol = 10, nrow = 10)
  
  # type n: calculates number of participants for each group (see File 1. )
  if (type == "n") {
    for (i in 1:9) {
      for (j in 1:9) {
        d <- data[(data$docent_advies == i & data$toets_advies == j), ]
        prop[i, j] <- nrow(d)
        f <- data[(data$toets_advies == j), ]
        prop[10, j] <- round((sum(f$overeenkomst2) / nrow(f)), 3)
      }
      e <- data[(data$docent_advies == i), ]
      prop[i, 10] <- round((sum(e$overeenkomst2) / nrow(e)), 3)
    }
    prop[10, 10] <- round((sum(data$overeenkomst2) / nrow(data)), 3)
  }
  
  else {
    # determine if there is any overlap (depending on type; overlap between optimal placement and TBR EPST reccomendation, optimal placement and RR EPST or RR EPTS and TBR EPST)
    if (type == "true") {
      data <- data %>%
        mutate(overeenkomst =  if_else((
            (as.numeric(as.character(plaatsing)) == as.numeric(as.character(design_advies)) )|
            (as.numeric(as.character(plaatsing)) == as.numeric(as.character(design_advies)) + 1)|
            (as.numeric(as.character(plaatsing)) == as.numeric(as.character(design_advies)) - 1)  |
            (as.numeric(as.character(plaatsing)) == 3 & (as.numeric(as.character(plaatsing)) == as.numeric(as.character(design_advies)) + 2)) |
            (as.numeric(as.character(plaatsing)) == 5 & (as.numeric(as.character(plaatsing)) == as.numeric(as.character(design_advies)) - 2 ))
        ), 1, 0 ))
    }
    else if (type == "EPST")
    {
      data <- data %>%
        mutate(overeenkomst =  if_else((
            (as.numeric(as.character(plaatsing)) == as.numeric(as.character(toets_advies)) )|
            (as.numeric(as.character(plaatsing)) == as.numeric(as.character(toets_advies)) + 1) |
            (as.numeric(as.character(plaatsing)) == as.numeric(as.character(toets_advies)) - 1 ) |
            (as.numeric(as.character(plaatsing)) == 3 & (as.numeric(as.character(plaatsing)) == as.numeric(as.character(toets_advies)) + 2)) |
            (as.numeric(as.character(plaatsing)) == 5 & (as.numeric(as.character(plaatsing)) == as.numeric(as.character(toets_advies)) - 2 ))
        ), 1, 0 ))
    }

    # Given the calculated overlap; determine for each comination of teacher advice and optimal track placement the percentage of students who have overlapping recommendations
    for (i in 1:9) {
      for (j in 1:9)
      {
        d <- data[(data$docent_advies == i & data$plaatsing == j), ]
        if (nrow(d) == 0) {
          prop[i, j] <- "0 cases"
        }
        else{
          prop[i, j] <- round((sum(d$overeenkomst) / nrow(d)), 3)
        }
        f <- data[(data$plaatsing == j), ]
        prop[10, j] <- round((sum(f$overeenkomst) / nrow(f)), 3)
      }
      e <- data[(data$docent_advies == i), ]
      prop[i, 10] <- round((sum(e$overeenkomst) / nrow(e)), 3)
    }
    prop[10, 10] <- round((sum(data$overeenkomst) / nrow(data)), 4)
  }
  prop <- prop[-4, -4]
  colnames(prop)<- c("bb", "bb_kb", "kb", "gt", "gt_havo", "havo", "havo_vwo", "vwo", "marg")
  rownames(prop) <- c("bb", "bb_kb", "kb", "gt", "gt_havo", "havo", "havo_vwo", "vwo", "marg") 
  return(prop)
}
