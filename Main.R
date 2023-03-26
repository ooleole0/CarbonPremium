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
        prccm,
        cshom,
        trfm,
        trt1m,
        cshtrm
    ) %>%
    filter(exchg == c(10 , 11)) %>%
    # correct the return units as they are of percentages
    mutate(
        mktcap = abs(prccm) * cshom,
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

# merge breakpoints
stock <- stock %>%
    left_join(scope1_2_breakpoints, by = "sorting_year") %>%
    left_join(scope1_2_grow_breakpoints, by = "sorting_year") %>%
    left_join(scope1_2_grow_r_breakpoints, by = "sorting_year") %>%
    left_join(scope1_2_3_breakpoints, by = "sorting_year") %>%
    left_join(inten_rev_1_2_grow_r_breakpoints, by = "sorting_year") %>%
    left_join(inten_asset_1_2_grow_r_breakpoints, by = "sorting_year")

# flag groups
stock <- stock %>%
    mutate(scope1_2_type = case_when(scope1_2 <= scope1_2_q20 ~ '1',
                                     scope1_2 > scope1_2_q20 &
                                         scope1_2 <= scope1_2_q40 ~ '2',
                                     scope1_2 > scope1_2_q40 &
                                         scope1_2 <= scope1_2_q60 ~ '3',
                                     scope1_2 > scope1_2_q60 &
                                         scope1_2 <= scope1_2_q80 ~ '4',
                                     scope1_2 > scope1_2_q80 ~ '5'),
           scope1_2_grow_type = case_when(scope1_2_grow <=
                                              scope1_2_grow_q20 ~ '1',
                                          scope1_2_grow > scope1_2_grow_q20 &
                                              scope1_2_grow <=
                                              scope1_2_grow_q40 ~ '2',
                                          scope1_2_grow > scope1_2_grow_q40 &
                                              scope1_2_grow <=
                                              scope1_2_grow_q60 ~ '3',
                                          scope1_2_grow > scope1_2_grow_q60 &
                                              scope1_2_grow <=
                                              scope1_2_grow_q80 ~ '4',
                                          scope1_2_grow >
                                              scope1_2_grow_q80 ~ '5'),
           scope1_2_grow_r_type = case_when(scope1_2_grow_r <=
                                                scope1_2_grow_r_q20 ~ '1',
                                          scope1_2_grow_r > scope1_2_grow_r_q20 &
                                              scope1_2_grow_r <=
                                              scope1_2_grow_r_q40 ~ '2',
                                          scope1_2_grow_r > scope1_2_grow_r_q40 &
                                              scope1_2_grow_r <=
                                              scope1_2_grow_r_q60 ~ '3',
                                          scope1_2_grow_r > scope1_2_grow_r_q60 &
                                              scope1_2_grow_r <=
                                              scope1_2_grow_r_q80 ~ '4',
                                          scope1_2_grow_r >
                                              scope1_2_grow_r_q80 ~'5'),
           scope1_2_3_type = case_when(scope1_2_3 <= scope1_2_3_q20 ~ '1',
                                       scope1_2_3 > scope1_2_3_q20 &
                                           scope1_2_3 <= scope1_2_3_q40 ~ '2',
                                       scope1_2_3 > scope1_2_3_q40 &
                                           scope1_2_3 <= scope1_2_3_q60 ~ '3',
                                       scope1_2_3 > scope1_2_3_q60 &
                                           scope1_2_3 <= scope1_2_3_q80 ~ '4',
                                       scope1_2_3 > scope1_2_3_q80 ~ '5'),
           inten_rev_1_2_grow_r_type = case_when(inten_rev_1_2_grow_r <=
                                                     inten_rev_1_2_grow_r_q20 ~ '1',
                                            inten_rev_1_2_grow_r >
                                                inten_rev_1_2_grow_r_q20 &
                                                inten_rev_1_2_grow_r <=
                                                inten_rev_1_2_grow_r_q40 ~ '2',
                                            inten_rev_1_2_grow_r >
                                                inten_rev_1_2_grow_r_q40 &
                                                inten_rev_1_2_grow_r <=
                                                inten_rev_1_2_grow_r_q60 ~ '3',
                                            inten_rev_1_2_grow_r >
                                                inten_rev_1_2_grow_r_q60 &
                                                inten_rev_1_2_grow_r <=
                                                inten_rev_1_2_grow_r_q80 ~ '4',
                                            inten_rev_1_2_grow_r >
                                                inten_rev_1_2_grow_r_q80 ~ '5'),
           inten_asset_1_2_grow_r_type = case_when(inten_asset_1_2_grow_r <=
                                                       inten_asset_1_2_grow_r_q20 ~ '1',
                                                   inten_asset_1_2_grow_r >
                                                       inten_asset_1_2_grow_r_q20 &
                                                       inten_asset_1_2_grow_r <=
                                                       inten_asset_1_2_grow_r_q40 ~ '2',
                                                   inten_asset_1_2_grow_r >
                                                       inten_asset_1_2_grow_r_q40 &
                                                       inten_asset_1_2_grow_r <=
                                                       inten_asset_1_2_grow_r_q60 ~ '3',
                                                   inten_asset_1_2_grow_r >
                                                       inten_asset_1_2_grow_r_q60 &
                                                       inten_asset_1_2_grow_r <=
                                                       inten_asset_1_2_grow_r_q80 ~ '4',
                                                   inten_asset_1_2_grow_r >
                                                       inten_asset_1_2_grow_r_q80 ~ '5')
    )

# pick the needed "type" variables
data_typed <- stock %>%
    select(datadate.x,
           gvkey,
           trt1m,
           mktcap,
           scope1_2_type,
           scope1_2_grow_type,
           scope1_2_grow_r_type,
           scope1_2_3_type,
           inten_rev_1_2_grow_r_type,
           inten_asset_1_2_grow_r_type
           ) %>%
    group_by(gvkey) %>%
    mutate(weight = lag(mktcap)) %>%
    ungroup()

# construct new weighted mean function since weighted.mean doesn't handle 
# NA weights
weighted_mean = function(x, w, ..., na.rm = F){
    if(na.rm){
        keep = !is.na(x)&!is.na(w)
        w = w[keep]
        x = x[keep]
    }
    weighted.mean(x, w, ..., na.rm = F)
}

# valuate different portfolios' returns
portf_scope1_2 <- data_typed %>% 
    group_by(datadate.x, scope1_2_type) %>% 
    summarise(vwret = weighted_mean(trt1m, w = weight, na.rm = T)) %>%
    ungroup()

portf_scope1_2 <- portf_scope1_2 %>% 
    pivot_wider(
        id_cols = datadate.x,
        values_from = vwret,
        names_from = c(scope1_2_type),
        names_sep = ""
    )

portf_scope1_2_grow <- data_typed %>% 
    group_by(datadate.x, scope1_2_grow_type) %>% 
    summarise(vwret = weighted_mean(trt1m, w = weight, na.rm = T)) %>%
    ungroup()

portf_scope1_2_grow <- portf_scope1_2_grow %>% 
    pivot_wider(
        id_cols = datadate.x,
        values_from = vwret,
        names_from = c(scope1_2_grow_type),
        names_sep = ""
    )

portf_scope1_2_grow_r <- data_typed %>% 
    group_by(datadate.x, scope1_2_grow_r_type) %>% 
    summarise(vwret = weighted_mean(trt1m, w = weight, na.rm = T)) %>%
    ungroup()

portf_scope1_2_grow_r <- portf_scope1_2_grow_r %>% 
    pivot_wider(
        id_cols = datadate.x,
        values_from = vwret,
        names_from = c(scope1_2_grow_r_type),
        names_sep = ""
    )

portf_scope1_2_3 <- data_typed %>% 
    group_by(datadate.x, scope1_2_3_type) %>% 
    summarise(vwret = weighted_mean(trt1m, w = weight, na.rm = T)) %>%
    ungroup()

portf_scope1_2_3 <- portf_scope1_2_3 %>% 
    pivot_wider(
        id_cols = datadate.x,
        values_from = vwret,
        names_from = c(scope1_2_3_type),
        names_sep = ""
    )

portf_inten_rev_1_2_grow_r <- data_typed %>% 
    group_by(datadate.x, inten_rev_1_2_grow_r_type) %>% 
    summarise(vwret = weighted_mean(trt1m, w = weight, na.rm = T)) %>%
    ungroup()

portf_inten_rev_1_2_grow_r <- portf_inten_rev_1_2_grow_r %>% 
    pivot_wider(
        id_cols = datadate.x,
        values_from = vwret,
        names_from = c(inten_rev_1_2_grow_r_type),
        names_sep = ""
    )

portf_inten_asset_1_2_grow_r <- data_typed %>% 
    group_by(datadate.x, inten_asset_1_2_grow_r_type) %>% 
    summarise(vwret = weighted_mean(trt1m, w = weight, na.rm = T)) %>%
    ungroup()

portf_inten_asset_1_2_grow_r <- portf_inten_asset_1_2_grow_r %>% 
    pivot_wider(
        id_cols = datadate.x,
        values_from = vwret,
        names_from = c(inten_asset_1_2_grow_r_type),
        names_sep = ""
    )