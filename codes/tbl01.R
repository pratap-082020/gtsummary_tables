#---------------------------------------------------------
# Program Name : tbl01
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
  ~studyid,~usubjid,~trt01pn,~age,~fasfl,~trt01p,
  "MYCSG","MYCSG-1001",1,23,"Y","Dose level 1",
  "MYCSG","MYCSG-1002",3,68,"Y","Dose level 3",
  "MYCSG","MYCSG-1003",3,NA,"Y","Dose level 3",
  "MYCSG","MYCSG-1004",3,35,"Y","Dose level 3",
  "MYCSG","MYCSG-1006",3,54,"Y","Dose level 3",
  "MYCSG","MYCSG-1007",1,63,"N","Dose level 1",
)



adsl_fas <- adsl %>% filter(fasfl == "Y")

tbl <- adsl_fas %>%
  select(trt01p, age) %>%
  tbl_summary(
    by = trt01p,
    type = age ~ "continuous2",
    statistic = age ~ c(
      "{N_nonmiss} ({N_miss})",
      "{mean} ({sd})",
      "{median}",
      "{p25}, {p75}",
      "{min}, {max}"
    ),
    digits = age ~ c(0, 1, 1, 1, 0),
    missing = "no"
  ) %>%
  add_overall(last = TRUE, col_label = "**Total**") %>%
  modify_header(label ~ "**Statistic**") %>%
  modify_table_body(~ .x %>%
                      mutate(
                        label = c(
                          "Age (years)",
                          "n (missing)",
                          "Mean (SD)",
                          "Median",
                          "Q1, Q3",
                          "Min, Max"
                        )
                      )) %>%
  bold_labels() %>%
  modify_caption(
    "**Table 14.1.1 Demographic Characteristics<br>Full Analysis Set**"
  ) %>% modify_header(
    stat_1 ~ "**Dose level 1**",
    stat_2 ~ "**Dose level 3**"
  )

tbl

gt_tbl <- as_gt(tbl)
gtsave(
  gt_tbl,
  "output/tbl01.pdf"
)
