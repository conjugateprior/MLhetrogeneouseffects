#load data
library(haven)
library(caret)
library(tidyverse)
library(dplyr)

Uganda_Data <- read_dta("~/Desktop/Master thesis/MLhetrogeneouseffects/data/Uganda ELA Panel wide.dta")
View(Uganda_Data)

#* generating variables
#gen age_Imarry=M_marrywhen+age
#replace age_Imarry=. if age_Imarry<16 | age_Imarry>50
#gen Rage_Imarry=RM_marrywhen
#replace Rage_Imarry=. if Rage_Imarry<16 | Rage_Imarry>50
#gen M_marrywhox=M_marrywho
#gen RM_marrywhox=RM_marrywho

#generate variables
Uganda_Data <- Uganda_Data %>% transform(
                     age_Imarry = M_marrywhen + age, 
                     Rage_Imarry = RM_marrywhen)

#replace with na values: this doesn't work yet!!!
Uganda_Data <- Uganda_Data %>% select(
  age_Imarry, Rage_Imarry) %>%
  mutate(age_Imarry = na_if(age_Imarry, gt(50)),
         Rage_Imarry = na_if(Rage_Imarry, gt(50)))
                     
?na_if

names(Uganda_Data)
Uganda_Data$age_Imarry
