# ============================================================
# Downloaded from myCSG lesson content
# Lesson: TFL_TABGEN_L201
# Content Type: r_program
# Downloaded By: Ravi
# Email: ppm08641@gmail.com
# ============================================================

#source("D:/SAS/Home/dev/clinical_sas_samples/mycsg/mycsg_config.R")
#rmac1-std1
#rmac1-std2
#rmac1-std3
 
 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 
 
library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(huxtable)
library(data.table)
library(hms)
library(lubridate)
library(pharmaRTF)
 
 
source("./TFL_TABGEN_L201_data.r")
 
 
#==============================================================================;
#subset required records;
#==============================================================================;
 
adsl01<-adsl %>%
 filter(fasfl=="Y")
 
#==============================================================================;
#replicate rows for total column;
#==============================================================================;
 
adsl02<-adsl01 %>%
 mutate(treatment=trt01pn) %>%
 bind_rows(adsl01 %>% mutate(treatment=4))
 
#==============================================================================;
#Calculate summary statistics;
#==============================================================================;
 
stats01 <- adsl02 %>%
 group_by(treatment) %>%
 summarize(
 nrecs = n(),
 age_nmiss = sum(is.na(age)),
 age_n = nrecs-age_nmiss,
 age_mean = mean(age, na.rm = TRUE),
 age_stddev = sd(age, na.rm = TRUE),
 age_min = min(age, na.rm = TRUE),
 age_q1 = quantile(age, 0.25, type=2, na.rm = TRUE),
 age_median = median(age, na.rm = TRUE),
 age_q3 = quantile(age, 0.75, type=2, na.rm = TRUE),
 age_max = max(age, na.rm = TRUE)
 ) %>%
 complete(treatment = 1:4,
 fill = list(age_n = 0, age_nmiss = 0, age_mean = NA, age_std = NA,
 age_min = NA, age_q1 = NA, age_median = NA,
 age_q3 = NA, age_max = NA))
 
#==============================================================================;
#Process the statistics as per presentation requirements;
#==============================================================================;
 
stats02 <- stats01 %>%
 mutate(
 mean = ifelse(!is.na(age_mean), sprintf("%.1f", age_mean), NA_character_),
 std = ifelse(!is.na(age_stddev), paste0(" (", sprintf("%.2f", age_stddev), ")"), " ( - )"),
 median = ifelse(!is.na(age_median), sprintf("%.1f", age_median), NA_character_),
 q1 = ifelse(!is.na(age_q1), sprintf("%.1f", age_q1), NA_character_),
 q3 = ifelse(!is.na(age_q3), sprintf("%.1f", age_q3), NA_character_),
 min = ifelse(!is.na(age_min), sprintf("%.0f", age_min), NA_character_),
 max = ifelse(!is.na(age_max), sprintf("%.0f", age_max), NA_character_),
 nnmiss = paste0(formatC(age_n, format = "d", width = 3), " (", formatC(age_nmiss, format = "d", width = 3), ")"),
 meanstd = ifelse(age_n != 0, paste0(trimws(mean), trimws(std)), NA_character_),
 q1q3 = ifelse(age_n != 0, paste0(trimws(q1), ",", trimws(q3)), NA_character_),
 minmax = ifelse(age_n != 0, paste0(trimws(min), ",", trimws(max)), NA_character_)
 )
 
stats03 <- stats02 %>%
 select(treatment, nnmiss, meanstd, q1q3, median, minmax)
 
#==============================================================================;
#Restructure the statistics such that they appear as 'rows' ;
#==============================================================================;
 
stats04 <- stats03 %>%
 pivot_longer(cols = c(nnmiss, meanstd, median, q1q3, minmax),
 names_to = "name",
 values_to = "col1")
#==============================================================================;
#Create some supporting variables as per sorting and presentation requirements;
#==============================================================================;
 
stats05 <- stats04 %>%
 mutate(
 group = 1,
 grouplabel = "Age (years)",
 name = toupper(name),
 intord = case_when(
 name == "NNMISS" ~ 1,
 name == "MEANSTD" ~ 2,
 name == "MEDIAN" ~ 3,
 name == "Q1Q3" ~ 4,
 name == "MINMAX" ~ 5
 ),
 statistic = case_when(
 name == "NNMISS" ~ "n (missing)",
 name == "MEANSTD" ~ "Mean (SD)",
 name == "MEDIAN" ~ "Median",
 name == "Q1Q3" ~ "Q1, Q3",
 name == "MINMAX" ~ "Min, Max"
 )
 ) %>%
 select(-name)
 
#==============================================================================;
#Restructure the data such that treatments appear as 'columns';
#==============================================================================;
 
stats06 <- stats05 %>%
 pivot_wider(
 id_cols=c(group,grouplabel,intord,statistic),
 names_from = treatment,
 values_from = col1,
 names_prefix = "trt"
 ) %>%
 mutate(across(c(trt1,trt2,trt3,trt4),~if_else(is.na(.),"",.)))
 
 
output<-stats06
 
#==============================================================================;
#datsets to excel file;
#==============================================================================;
 


# ============================================================
# End of downloaded content
# Downloaded By: Ravi
# Email: ppm08641@gmail.com
# ============================================================