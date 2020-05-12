######
# F6. Allocation()
#####

## ---------------------------------------------------------------------------------------------------------------------------------

# Allocate each student to a module based on the teachers recommendation 
Allocation <- function(dat, grens){
  require(tidyverse)
  if(length(grens)==0){
    new_data <- dat %>% 
      mutate(module = 1)
  }
  else if(length(grens)==1){
    new_data <- dat %>% 
      mutate(module = case_when(
        docent_advies <= grens[1] ~ 1,
        docent_advies > grens[1] ~ 2))
  }
  else if(length(grens)==2)
  {   new_data <- dat %>% 
    mutate(module = case_when(
      docent_advies <= grens[1] ~ 1,
      docent_advies > grens[1] & docent_advies <= grens[2] ~ 2,
      docent_advies > grens[2] ~ 3))
  }
  
  else if(length(grens)==3)
  {   new_data <- dat %>% 
    mutate(module = case_when(
      docent_advies <=  grens[1] ~ 1,
      docent_advies > grens[1] & docent_advies <= grens[2] ~ 2,
      docent_advies > grens[2] & docent_advies <= grens[3] ~ 3,
      docent_advies > grens[3] ~ 4))
  }
  else if(length(grens)==4)
  {   new_data <- dat %>% 
    mutate(module = case_when(
      docent_advies <=  grens[1] ~ 1,
      docent_advies > grens[1] & docent_advies <= grens[2] ~ 2,
      docent_advies > grens[2] & docent_advies <= grens[3] ~ 3,
      docent_advies > grens[3] & docent_advies <= grens[4] ~ 4,
      docent_advies > grens[4] ~ 5))
  }
  else if(length(grens)==5)
  {   new_data <- dat %>% 
    mutate(module = case_when(
      docent_advies <=  grens[1] ~ 1,
      docent_advies > grens[1] & docent_advies <= grens[2] ~ 2,
      docent_advies > grens[2] & docent_advies <= grens[3] ~ 3,
      docent_advies > grens[3] & docent_advies <= grens[4] ~ 4,
      docent_advies > grens[4] & docent_advies <= grens[5] ~ 5,
      docent_advies > grens[5] ~ 6))
  }
  else if(length(grens)==6)
  {   new_data <- dat %>% 
    mutate(module = case_when(
      docent_advies <=  grens[1] ~ 1,
      docent_advies > grens[1] & docent_advies <= grens[2] ~ 2,
      docent_advies > grens[2] & docent_advies <= grens[3] ~ 3,
      docent_advies > grens[3] & docent_advies <= grens[4] ~ 4,
      docent_advies > grens[4] & docent_advies <= grens[5] ~ 5,
      docent_advies > grens[5] & docent_advies <= grens[6] ~ 6,
      docent_advies > grens[6] ~ 7))
  }
  else if(length(grens)==7)
  {   new_data <- dat %>% 
       mutate(module = case_when(
      docent_advies <=  grens[1] ~ 1,
      docent_advies > grens[1] & docent_advies <= grens[2] ~ 2,
      docent_advies > grens[2] & docent_advies <= grens[3] ~ 3,
      docent_advies > grens[3] & docent_advies <= grens[4] ~ 4,
      docent_advies > grens[4] & docent_advies <= grens[5] ~ 5,
      docent_advies > grens[5] & docent_advies <= grens[6] ~ 6,
      docent_advies > grens[6] & docent_advies <= grens[7] ~ 7,
      docent_advies > grens[7] ~ 8))
  }
  else
  {
      new_data <- dat
      print("too many treshold values")
  }

  return(new_data)
}


