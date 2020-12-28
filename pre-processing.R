############
# PACKAGES #
############

library(haven)
library(caret)
library(tidyverse)
library(expss)
library(summarytools)
library(mice)


#############
# LOAD DATA #
#############

ud <- read_dta("Uganda ELA Panel wide_Creation.dta")

##################
# PRE PROCESSING #
##################

##removes variables that are were used to create indexes 
##keeping all indexes 
ud_scales <- ud %>% 
  subset(select = c("HHAssetvalue_total", "HHF_loanbrac", "HHM_whoshouldearn","M_ablework_if",
                    "M_children", "M_marrywhen", "M_marrywho", "M_wanttowork_if", "QC_clubheard", 
                    "QC_clubparticipateIMP", "QC_stillgoing", "QE_Denrolled", "Qage", "Qback_school",
                    "Qhsworked_year_empl","Qhsworked_year_self", "Qincind_empl", "Qincind_selfempl",
                    "Qincome_year_ind", "QlifeskillMOREfew", "QlifeskillMOREfewIMP","QlivelihoodMOREfew",
                    "QlivelihoodMOREfewIMP", "Qoften1WEEK", "Qoften3WEEK", "Qstudy_hours", "Qworry_job", 
                    "RC_clubheard","RC_clubparticipateIMP","RC_stillgoing","RE_Denrolled", "Rage", "Rback_school",
                    "Rhsworked_year_self", "Rhsworked_year_empl", "Rincind_empl", "Rincind_selfempl", "Rstudy_hours",
                    "Rworry_job", "age", "back_school","baseline", "branch_name", "branchno", "dist_nearclub","endline", 
                    "follow_up", "hsworked_year_empl", "hsworked_year_self", "id", "idno","incind_empl", "incind_selfempl",
                    "income_year_ind", "lifeskillMOREfew", "lifeskillMOREfewIMP","livelihoodMOREfew","livelihoodMOREfewIMP",
                    "often1WEEK","often3WEEK", "satisfaction_income", "selfempl","study_hours",
                    "treatment", "villid", "worry_job", "age_Imarry", "Rage_Imarry",          
                    "rural", "rich","below16","z_QEntrep_total", "z_Qany_iga", "z_Qselfempl", "z_Qempl","z_QExpenditure_totDF", 
                    "QM_chi","Qpart", "z_QM_chi","z_Qpart","z_QR_sexu","z_Qsex_p", "z_QRhiv_s", "z_Qalways_c", "z_Qother_c", "QM_baby_no",           
                    "QM_son", "z_Qempowerment", "z_QM_i_ageF","z_QM_i_ageM","z_QM_baby_no","z_QM_baby_ageF",       
                    "z_QM_daught", "z_QM_son","Qcontrol_body","Qaspiration","Qiga","z_REntrep_total","z_Rany_iga","z_Rselfempl","z_Rempl",               
                    "z_RExpenditure_totDF", "RM_chi", "Rpart", "RR_sexu", "Ralways_c","Rother_c","z_RM_chi",              
                    "z_Rpart", "z_RR_sexu", "z_Rsex_p", "z_RRhiv_s", "z_Ralways_c","z_Rother_c",
                    "z_Rempowerment","z_RM_i_ageF","z_RM_i_ageM","z_RM_baby_no","z_RM_baby_ageF","z_RM_daught","z_RM_son",
                    "Rcontrol_body","Raspiration","Riga", "z_Entrep_total","z_any_iga", "z_selfempl","z_empl","z_Expenditure_totDF",
                    "zALL_Entrep_total","zALL_any_iga","zALL_selfempl","zALL_empl", "zALL_Expenditure_totDF", "M_chi","part",                
                    "always_c","other_c","z_M_chi", "z_part","z_R_sexu","z_sex_p","z_Rhiv_s","z_always_c","z_other_c","zALL_R_sexu",
                    "zALL_sex_p", "zALL_Rhiv_s","zALL_always_c","zALL_other_c","z_empowerment","z_M_i_ageF","z_M_i_ageM","z_M_baby_no",
                    "z_M_baby_ageF","z_M_daught","z_M_son","zALL_empowerment","zALL_M_i_ageF","zALL_M_i_ageM", 
                    "zALL_M_baby_no","zALL_M_baby_ageF","zALL_M_daught", "zALL_M_son","control_body","aspiration", 
                    "iga","igaALL", "_Bbranch_na_2","_Bbranch_na_3","_Bbranch_na_4", "_Bbranch_na_5", "_Bbranch_na_6","_Bbranch_na_7", 
                    "_Bbranch_na_8","_Bbranch_na_9", "_Bbranch_na_10")) 


#Function to standardize variables
standardize <- function(data) {
  mean_data <- mean(data)
  sd_data <- sd(data)
  stand_data <- (data - mean_data)/sd_data
  return(stand_data)
}

#Standardize variables
ud_scales$z_hsworked_year_empl <- standardize(ud_scales$hsworked_year_empl)
ud_scales$z_Qhsworked_year_empl <- standardize(ud_scales$Qhsworked_year_empl)
ud_scales$z_Rhsworked_year_empl <- standardize(ud_scales$Rhsworked_year_empl)
#ud_scales$z_satisfaction_income <- standardize(ud_scales$satisfaction_income) #NULL
#ud_scales$z_study_hours <- standardize(ud_scales$study_hours) #NULL
#ud_scales$z_Qstudy_hours <- standardize(ud_scales$Qstudy_hours) #NULL
#ud_scales$z_Rstudy_hours <- standardize(ud_scales$Rstudy_hours) #NULL
#ud_scales$z_age_Imarry <- standardize(ud_scales$age_Imarry) #NULL
#ud_scales$z_Rage_Imarry <- standardize(ud_scales$Rage_Imarry) #NULL
#ud_scales$z_M_marrywho <- standardize(ud_scales$M_marrywho) #NULL










###############
# DESCRIPTIVE #
###############

# Step 1: understand how many na in each row (observarion) 
obs_na <- apply(ud, MARGIN = 1, function(x) sum(is.na(x)))
obs_na <- data.frame(obs_na)
obs_na_alot <- obs_na %>%
  filter(obs_na>99)

# Step 2: understand how many na in each column (variable) 
var_na <- md.pattern(ud)

# Step 3: descriptive stats
descriptive <- dfSummary(ud)
view(descriptive)










library(VIM)
mice_plot <- aggr(ud, col=c('navyblue','purple'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(ud), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

mice_plot$missing



# Customised Function for summarystats: currently not working
custom_glimpse <- function(df_summary) {
  data.frame(
    col_name = colnames(df_summary),
    col_index = 1:ncol(df_summary),
    col_class = sapply(df_summary, class= c(double, numeric)),
    col_mean = sapply(df_summary, mean),
    col_obs = sapply(df_summary, count),
    row.names = NULL,
    return(df_summary)
  )
}


# Generates Data set with no NA
ud_subset_na <- as.data.frame(na.omit(apply(ud_subset,2,function (x) x[order(is.na(x))])))



