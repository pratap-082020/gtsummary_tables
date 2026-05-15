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
  ~studyid,~usubjid,~trt01pn,~age,~height,~asex,~fasfl,~trt01p,
  "MYCSG","MYCSG-1001",1,23,165,"Female","Y","Dose level 1",
  "MYCSG","MYCSG-1002",3,68,NA,"","Y","Dose level 3",
  "MYCSG","MYCSG-1003",3,NA,153,"Male","Y","Dose level 3",
  "MYCSG","MYCSG-1004",3,35,152,"Female","Y","Dose level 3",
  "MYCSG","MYCSG-1006",3,54,172,"Male","Y","Dose level 3",
  "MYCSG","MYCSG-1007",3,59,45,"Female","N","Dose level 3",
)



gt_tbl <- adsl %>% filter(fasfl=="Y") %>% select(asex, trt01p) %>%
  gtsummary::tbl_summary(
  by=trt01p,
  statistic = all_categorical() ~ "{n} ({p}%)",
  digits = all_categorical() ~ c(0, 1)) %>%
  add_overall(last = TRUE, col_label = "**Total<br>(N = {n})**") %>%
  modify_header(label ~ "**Statistic**",
                stat_1 ~ "**Dose level 1<br>(N = {n})**",
                stat_2 ~ "**Dose level 3<br>(N = {n})**"
                ) %>%
  modify_caption(
    "**Summary of categorical variables<br>Full Analysis Set**"
  ) %>% modify_table_body(
    ~ .x %>% filter(label != "n (%)") %>%
      mutate(
        label = ifelse(label == "asex", "Sex, n (%)", label),
        label = ifelse(label == "", "Missing", label)
      )
  ) %>% modify_footnote(all_stat_cols() ~ NA)
  
gt_tbl

gt_tbl <- as_gt(gt_tbl)
gtsave(
  gt_tbl,
  "output/tbl03.pdf"
)
