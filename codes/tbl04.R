#---------------------------------------------------------
# Program Name : tbl03
# Study        : 
# Purpose      : Descriptive statistics for Age variable
# Author       : Vijay Pratap
# Created Date : 
# Input        : 
# Output       : 
#---------------------------------------------------------

packages <- c("tibble", "dplyr", "gtsummary", "gt")

for(pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

adsl<-tribble(
  ~studyid,~usubjid,~trt01pn,~age,~height,~asex,~arace,~fasfl,~trt01p,
  "MYCSG","MYCSG-1001",1,23,165,"Female","Asian","Y","Dose level 1",
  "MYCSG","MYCSG-1002",3,68,NA,"","Black or African American","Y","Dose level 3",
  "MYCSG","MYCSG-1003",3,NA,153,"Male","","Y","Dose level 3",
  "MYCSG","MYCSG-1004",3,35,152,"Female","White","Y","Dose level 3",
  "MYCSG","MYCSG-1006",3,54,172,"Male","Not reported","Y","Dose level 3",
  "MYCSG","MYCSG-1007",3,54,180,"Male","White","N","Dose level 3",
)


tbl <- adsl %>%
  filter(fasfl == "Y") %>%   # Full Analysis Set
  select(trt01p, asex, arace) %>% mutate(
    asex = if_else(asex=="", "Missing", asex),
    arace = if_else(arace=="", "Missing", arace)
  ) %>%
  tbl_summary(
    by = trt01p,
    missing = "ifany",
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"   # show both frequency and percent
    )
  ) %>% add_overall(last = T, col_label="**Total<br>(N = {n})**") %>%
  modify_table_body(
    ~.x %>% mutate(
      label = if_else(label == "asex", "Sex, n (%)", label),
      label = if_else(label == "arace", "Race, n (%)", label)
    )
  ) %>% modify_footnote(all_stat_cols() ~ NA) %>%
  modify_header(
    label ~ "**Statistic**",
    stat_1 ~ "**Dose level 1<br>(N = {n})**",
    stat_2 ~ "**Dose level 3<br>(N = {n})**"
  ) %>%
  modify_caption(
    "**Summary for categorical variables<br>Full Analysis Set**"
  )
  
  
tbl



