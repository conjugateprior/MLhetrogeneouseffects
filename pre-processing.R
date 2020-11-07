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

#replace with na values

names(Uganda_Data)
Uganda_Data$M
