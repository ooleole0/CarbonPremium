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

SIC_sheet <- read_csv("SIC_sheet.csv", col_types = "c", na="")

carb_fin <- read_csv("financial.csv", na="") %>%
    select(
        gvkey,
        sic,
        datadate,
        fyear,
        act,
        at,
        revt
    ) %>%
    left_join(carbon, by = c("gvkey", "fyear")) %>%
    left_join(SIC_sheet, by = "sic") %>%
    # create carbon-related variables
    mutate(
        scope1_2 = scope1 + scope2,
        scope1_2_3 = scope1 + scope2 + scope3_up,
        inten_rev_1_2 = scope1_2 / revt,
        inten_rev_1_2_3 = scope1_2_3 / revt,
        inten_asset_1_2 = scope1_2 / at,
        inten_asset_1_2_3 = scope1_2_3 / at,
        inten_asset_long_1_2 = scope1_2 / (at - act),
        inten_asset_long_1_2_3 = scope1_2_3 / (at - act)
    ) %>%
    group_by(gvkey) %>%
    mutate(
        scope1_2_grow = scope1_2 - lag(scope1_2),
        scope1_2_grow_r = scope1_2_grow / lag(scope1_2),
        inten_rev_1_2_grow_r =
            (inten_rev_1_2 - lag(inten_rev_1_2)) / lag(inten_rev_1_2),
        inten_asset_1_2_grow_r =
            (inten_asset_1_2 - lag(inten_asset_1_2)) / lag(inten_asset_1_2),
        inten_asset_long_1_2_grow_r =
            (inten_asset_long_1_2 - lag(inten_asset_long_1_2)) /
            lag(inten_asset_long_1_2)
    ) %>%
    ungroup()

# remove unnecessary datasheets
remove(carbon, SIC_sheet)

stock <- read_csv("stock.csv", na = "")