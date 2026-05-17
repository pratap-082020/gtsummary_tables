#---------------------------------------------------------
# Program Name : tbl05
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
  ~studyid,~usubjid,~trt01pn,~age,~heightbl,~asex,~arace,~fasfl,~trt01p,
  "MYCSG","MYCSG-1001",1,23,165,"Female","Asian","Y","Dose level 1",
  "MYCSG","MYCSG-1002",3,68,NA,"","Black or African American","Y","Dose level 3",
  "MYCSG","MYCSG-1003",3,NA,153,"Male","","Y","Dose level 3",
  "MYCSG","MYCSG-1004",3,35,152,"Female","White","Y","Dose level 3",
  "MYCSG","MYCSG-1006",3,54,172,"Male","Not reported","Y","Dose level 3",
  "MYCSG","MYCSG-1007",3,45,165,"Male","Not reported","N","Dose level 3",
)



tbl01 <- adsl %>%
  filter(fasfl == "Y") %>%   
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
  ) %>% add_overall(last = T) %>%
  modify_table_body(
    ~.x %>% mutate(
      label = if_else(label == "asex", "Sex, n (%)", label),
      label = if_else(label == "arace", "Race, n (%)", label)
    )
  ) %>% modify_footnote(all_stat_cols() ~ NA)

  
  
tbl01



tbl02 <- adsl %>% filter(fasfl == "Y") %>%
  select(trt01p, age, heightbl) %>%
  tbl_summary(
    by = trt01p,
    type = c(age, heightbl) ~ "continuous2",
    statistic = c(age, heightbl) ~ c(
      "{N_nonmiss} ({N_miss})",
      "{mean} ({sd})",
      "{median}",
      "{p25}, {p75}",
      "{min}, {max}"
    ),
    digits = c(age, heightbl) ~ c(0, 1, 1, 1, 0),
    missing = "no"
  ) %>%
  add_overall() %>%
  modify_table_body(
    ~.x %>% mutate(
      label = if_else(label=="age", "Age (years)", label),
      label = if_else(label=="heightbl", "Height (cm)", label),
      label = if_else(label=="N Non-missing (N Missing)", "n (missing)", label)
    )
  )

tbl02




tbl01
tbl02

complete_tbl <- tbl_stack(list(tbl02, tbl01)) %>%
  modify_header(
    label ~ "**Characteristic**"
  ) %>%
  modify_caption(
    "**Demographic and Baseline Characteristics**<br>Full Analysis Set"
  )

complete_tbl

gt_tbl <- as_gt(complete_tbl)
gtsave(
  gt_tbl,
  "output/tbl05.pdf"
)



