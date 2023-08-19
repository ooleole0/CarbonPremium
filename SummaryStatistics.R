library(tidyverse)
library(stargazer)
library(vtable)
library(reshape2)
library(ggplot2)

vars <- read.csv("INDEVAR3_SORT.csv") %>%
    select(
        -s1IntSec,
        -s1_2IntSec,
        -s1_2_3IntSec
    )

carb_vars <- vars[, 10:23]
stargazer(
    carb_vars, 
    digits = 3,
    digits.extra = 3,
    type = "html", 
    no.space = TRUE, 
    out = "vars_summary.doc"
)