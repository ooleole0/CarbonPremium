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
        inten_rev_1_2 = scope1_2 / revt,
        inten_rev_1_2_3 = scope1_2_3 / revt,
        inten_asset_1_2 = scope1_2 / at,
        inten_asset_1_2_3 = scope1_2_3 / at,
        inten_asset_long_1_2 = scope1_2 / (at - act),
        inten_asset_long_1_2_3 = scope1_2_3 / (at - act)
    )
