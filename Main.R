library(tidyverse)
library(zoo)
library(lubridate)

carbon <- read_csv("carbon.csv", na="") %>%
    filter(country == 'United States') %>%
    select(companyid,
           gvkey,
           ticker,
           companyname,
           fyear,
           scope1,
           scope2,
           scope3_up
    )

carb_fin <- read_csv("financial.csv", na="") %>%
    select(
        gvkey,
        datadate,
        fyear,
        act,
        at,
        revt
    ) %>%
    left_join(carbon, by = c("gvkey", "fyear")) %>%
    mutate(
        scope1_2 = scope1 + scope2,
        scope1_2_3 = scope1 + scope2 + scope3_up,
        carb_inten_rev_1_2 = scope1_2 / revt,
        carb_inten_rev_1_2_3 = scope1_2_3 / revt
    )
