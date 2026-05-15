#---------------------------------------------------------
# Program Name : tbl02
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
  ~studyid,~usubjid,~trt01pn,~age,~heightbl,~fasfl,~trt01p,
  "MYCSG","MYCSG-1001",1,23,165,"Y","Dose level 1",
  "MYCSG","MYCSG-1002",3,68,NA,"Y","Dose level 3",
  "MYCSG","MYCSG-1003",3,NA,153,"Y","Dose level 3",
  "MYCSG","MYCSG-1004",3,35,152.5,"Y","Dose level 3",
  "MYCSG","MYCSG-1006",3,54,NA,"Y","Dose level 3",
  "MYCSG","MYCSG-1007",3,43,169,"N","Dose level 3",
)

gt_tbl <- adsl %>% filter(fasfl=="Y") %>% select(age, heightbl, trt01p) %>%
  gtsummary::tbl_summary(
  by=trt01p,
  type=c(age, heightbl)~"continuous2",
  missing = "no",
  statistic = c(age, heightbl) ~ c(
    "{N_nonmiss} ({N_miss})",
    "{mean} ({sd})",
    "{median}",
    "{p25}, {p75}",
    "{min}, {max}"
  ),
  digits = c(age, heightbl) ~ c(0, 0, 1, 1, 1, 1, 1, 0, 0)) %>%
  add_overall(last = TRUE, col_label = "**Total**") %>%
  modify_header(label ~ "**Statistic**") %>%
  modify_header(
    stat_1 ~ "**Dose level 1**",
    stat_2 ~ "**Dose level 3**"
  ) %>% modify_table_body(
    ~ .x %>%
      mutate(
        label = ifelse(
          label == "N Non-missing (N Missing)",
          "n (missing)",
          label
        ),
        label = ifelse(
          label == "age",
          "Age (years)",
          label
        ),
        label = ifelse(
          label == "heightbl",
          "Height (cm)",
          label
        )
      )
  ) %>%  modify_caption(
    "**Descriptive statistics for Age and Height<br>Full Analysis Set**"
  )
  

gt_tbl <- as_gt(gt_tbl)
gtsave(
  gt_tbl,
  "output/tbl02.pdf"
)
