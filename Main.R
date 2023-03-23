library(tidyverse)
library(zoo)
library(lubridate)

carbon <- read_csv("carbon.csv", na=c("","A","B","C")) %>%
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

carb_fin <- read_csv("financial.csv", na=c("", "A", "B", "C")) %>%
    select(
        gvkey,
        datadate,
        fyear,
        act,
        at,
        revt
    ) %>%
    left_join(carbon, by = c("gvkey", "fyear"))
