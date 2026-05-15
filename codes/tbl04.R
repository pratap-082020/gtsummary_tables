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



gt_tbl <- adsl %>%
  filter(fasfl == "Y") %>%
  select(asex, arace, trt01p) %>%
  tbl_summary(
    by = trt01p,
    missing = "ifany",
    type = list(
      arace ~ "continuous2",
      asex ~ "categorical"
    ),
    label = list(
      asex ~ "Sex, n (%)",
      arace ~ "Race"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      arace ~ c(
        "{N_nonmiss} ({N_miss})",
        "{mean} ({sd})",
        "{median}",
        "{p25}, {p75}",
        "{min}, {max}"
      )
    ),
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous2() ~ c(0, 1, 1, 1, 0)
    )
  )
  
gt_tbl

gt_tbl <- as_gt(gt_tbl)
gtsave(
  gt_tbl,
  "output/tbl03.pdf"
)
