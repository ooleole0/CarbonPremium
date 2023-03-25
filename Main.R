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

SIC_sheet <- read_csv("SIC_sheet.csv", na = "", col_types = "c")

carb_fin <- read_csv("financial.csv", na="") %>%
    select(
        gvkey,
        datadate,
        sic,
        fyear,
        act,
        at,
        revt,
        ebitda,
        epsfi,
        epsfx,
        epspi,
        epspx
    ) %>%
    left_join(SIC_sheet, by = "sic") %>%
    left_join(carbon, by = c("gvkey", "fyear")) %>%
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

stock <- read_csv("stock.csv", na = "")%>%
    select(
        gvkey,
        datadate,
        cyear,
        cmth,
        exchg,
        tic,
        cusip,
        prccm,
        trfm,
        trt1m,
        cshtrm
    ) %>%
    filter(exchg == c(10 , 11)) %>%
    # correct the return units as they are of percentages
    mutate(
        trfm = 0.01 * trfm,
        trt1m = 0.01 * trt1m,
        sorting_year = case_when(
            cmth <= 6 ~ cyear - 1,
            cmth >= 7 ~ cyear)
    )%>%
    left_join(carb_fin, by = join_by(gvkey, sorting_year == fyear)) %>%
    # create PE ratio variable
    mutate(
        pe_ratio = prccm / epspi
    ) %>%
    drop_na()

remove(carb_fin)

# create breakpoints for grouping
scope1_2_breakpoints <- stock %>%
    group_by(sorting_year) %>%
    summarise(
        scope1_2_q20 = quantile(scope1_2, 0.2, na.rm = TRUE),
        scope1_2_q40 = quantile(scope1_2, 0.4, na.rm = TRUE),
        scope1_2_q60 = quantile(scope1_2, 0.6, na.rm = TRUE),
        scope1_2_q80 = quantile(scope1_2, 0.8, na.rm = TRUE)
    )

scope1_2_grow_breakpoints <- stock %>%
    group_by(sorting_year) %>%
    summarise(
        scope1_2_grow_q20 = quantile(scope1_2_grow, 0.2, na.rm = TRUE),
        scope1_2_grow_q40 = quantile(scope1_2_grow, 0.4, na.rm = TRUE),
        scope1_2_grow_q60 = quantile(scope1_2_grow, 0.6, na.rm = TRUE),
        scope1_2_grow_q80 = quantile(scope1_2_grow, 0.8, na.rm = TRUE)
    )

scope1_2_grow_r_breakpoints <- stock %>%
    group_by(sorting_year) %>%
    summarise(
        scope1_2_grow_r_q20 = quantile(scope1_2_grow_r, 0.2, na.rm = TRUE),
        scope1_2_grow_r_q40 = quantile(scope1_2_grow_r, 0.4, na.rm = TRUE),
        scope1_2_grow_r_q60 = quantile(scope1_2_grow_r, 0.6, na.rm = TRUE),
        scope1_2_grow_r_q80 = quantile(scope1_2_grow_r, 0.8, na.rm = TRUE)
    )

scope1_2_3_breakpoints <- stock %>%
    group_by(sorting_year) %>%
    summarise(
        scope1_2_3_q20 = quantile(scope1_2_3, 0.2, na.rm = TRUE),
        scope1_2_3_q40 = quantile(scope1_2_3, 0.4, na.rm = TRUE),
        scope1_2_3_q60 = quantile(scope1_2_3, 0.6, na.rm = TRUE),
        scope1_2_3_q80 = quantile(scope1_2_3, 0.8, na.rm = TRUE)
    )

inten_rev_1_2_grow_r_breakpoints <- stock %>%
    group_by(sorting_year) %>%
    summarise(
        inten_rev_1_2_grow_r_q20 = 
            quantile(inten_rev_1_2_grow_r, 0.2, na.rm = TRUE),
        inten_rev_1_2_grow_r_q40 = 
            quantile(inten_rev_1_2_grow_r, 0.4, na.rm = TRUE),
        inten_rev_1_2_grow_r_q60 = 
            quantile(inten_rev_1_2_grow_r, 0.6, na.rm = TRUE),
        inten_rev_1_2_grow_r_q80 = 
            quantile(inten_rev_1_2_grow_r, 0.8, na.rm = TRUE)
    )

inten_asset_1_2_grow_r_breakpoints <- stock %>%
    group_by(sorting_year) %>%
    summarise(
        inten_asset_1_2_grow_r_q20 = 
            quantile(inten_asset_1_2_grow_r, 0.2, na.rm = TRUE),
        inten_asset_1_2_grow_r_q40 = 
            quantile(inten_asset_1_2_grow_r, 0.4, na.rm = TRUE),
        inten_asset_1_2_grow_r_q60 = 
            quantile(inten_asset_1_2_grow_r, 0.6, na.rm = TRUE),
        inten_asset_1_2_grow_r_q80 = 
            quantile(inten_asset_1_2_grow_r, 0.8, na.rm = TRUE)
    )

# merge breakpoints\
stock <- stock %>%
    left_join(scope1_2_breakpoints, by = "sorting_year") %>%
    left_join(scope1_2_grow_breakpoints, by = "sorting_year") %>%
    left_join(scope1_2_grow_r_breakpoints, by = "sorting_year") %>%
    left_join(scope1_2_3_breakpoints, by = "sorting_year") %>%
    left_join(inten_rev_1_2_grow_r_breakpoints, by = "sorting_year") %>%
    left_join(inten_asset_1_2_grow_r_breakpoints, by = "sorting_year")