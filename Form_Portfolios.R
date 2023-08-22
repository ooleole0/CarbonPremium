library(data.table)
library(stargazer)
library(dplyr)
library(tidyverse)

mrets = fread("CRSP_WEIGHT.csv")
vars = fread("INDEVAR3_SORT.csv") %>%
    select(
        -s1IntSec,
        -s1_2IntSec,
        -s1_2_3IntSec
    )


setkey(vars, sorting_year, gvkey)
vars = unique(vars, by=c("sorting_year","gvkey"))
vars[,.N,keyby=sorting_year]

June_Crsp = mrets[month(date)==6, .(sorting_year,date,gvkey)]
setkey(June_Crsp, sorting_year, gvkey)
June_Crsp <- June_Crsp[!is.na(gvkey)]

June_data <- merge(vars,June_Crsp)

June_data_tiled <- June_data %>% 
    group_by(sorting_year) %>%
    mutate(
        across(10:24, ~ntile(., 5))
    ) %>%
    ungroup() %>%
    as.data.table()

# N_Firms <- June_data_tiled %>% mutate(
#     across(11:27, ~n(.x))
# )
# 
# N_Firms = June_data_tiled[,.N,keyby=.(scope_1,sorting_year)]
# N_Firms = dcast(N_Firms,sorting_year~r,value.var="N")

#-----------------------------------------------------#

setkey(mrets,gvkey,sorting_year)
setkey(June_data_tiled,gvkey,sorting_year)

IndivStocks <- June_data_tiled %>%
    select(-date, -fyear_carb, -TotalRevenue) %>%
    left_join(mrets, by = c("sorting_year", "gvkey"), keep = FALSE) %>%
    select(-PERMNO, -COMNAM, ) %>%
    as.data.table()
setkey(IndivStocks,gvkey,date)
IndivStocks[is.na(weight),weight:=0]

wide <- function(df, var, var_name){
    df %>%
        drop_na({{var}}) %>%
        group_by(date, {{var}}) %>%
        summarise(vwret = weighted.mean(RET,w=weight,na.rm=T)) %>%
        ungroup() %>%
        pivot_wider(
            id_cols = date,
            names_from = {{var}},
            values_from = vwret,
            names_prefix = paste0(var_name, "Type")
        )
}

Portf_scope_1 <- wide(IndivStocks, scope_1, "scope_1")
Portf_scope_2 <- wide(IndivStocks, scope_2, "scope_2")
Portf_scope_3 <- wide(IndivStocks, scope_3, "scope_3")
Portf_scope_1_2 <- wide(IndivStocks, scope_1_2, "scope_1_2")
Portf_scope_1_2_3 <- wide(IndivStocks, scope_1_2_3, "scope_1_2_3")
Portf_scope_1_Int <- wide(IndivStocks, scope_1_Int, "scope_1_Int")
Portf_scope_1_2_Int <- wide(IndivStocks, scope_1_2_Int, "scope_1_2_Int")
Portf_scope_1_2_3_Int <- wide(IndivStocks, scope_1_2_3_Int, "scope_1_2_3_Int")
Portf_scope_1_grow <- wide(IndivStocks, scope_1_grow, "scope_1_grow")
Portf_scope_1_2_grow <- wide(IndivStocks, scope_1_2_grow, "scope_1_2_grow")
Portf_scope_1_2_3_grow <- wide(IndivStocks, scope_1_2_3_grow, "scope_1_2_3_grow")
Portf_s1IntSecDev <- wide(IndivStocks, s1IntSecDev, "s1IntSecDev")
Portf_s1_2IntSecDev <- wide(IndivStocks, s1_2IntSecDev, "s1_2IntSecDev")
Portf_s1_2_3IntSecDev <- wide(IndivStocks, s1_2_3IntSecDev, "s1_2_3IntSecDev")

Portf_list <- list(
    Portf_scope_1,
    Portf_scope_2,
    Portf_scope_3,
    Portf_scope_1_2,
    Portf_scope_1_2_3,
    Portf_scope_1_Int,
    Portf_scope_1_2_Int,
    Portf_scope_1_2_3_Int,
    Portf_scope_1_grow,
    Portf_scope_1_2_grow,
    Portf_scope_1_2_3_grow,
    Portf_s1IntSecDev,
    Portf_s1_2IntSecDev,
    Portf_s1_2_3IntSecDev
)
Portf <- Portf_list %>% reduce(full_join, by = "date")

Portf_LS <- Portf %>% 
    mutate(
    scope1_LS = scope_1Type5 - scope_1Type1,
    scope2_LS = scope_2Type5 - scope_2Type1,
    scope3_LS = scope_3Type5 - scope_3Type1,
    scope1_2_LS = scope_1_2Type5 - scope_1_2Type1,
    scope1_2_3_LS = scope_1_2_3Type5 - scope_1_2_3Type1,
    scope1Int_LS = scope_1_IntType5 - scope_1_IntType1,
    scope1_2Int_LS = scope_1_2_IntType5 - scope_1_2_IntType1,
    scope1_2_3Int_LS = scope_1_2_3_IntType5 - scope_1_2_3_IntType1,
    scope1Grow_LS = scope_1_growType5 - scope_1_growType1,
    scope1_2Grow_LS = scope_1_2_growType5 - scope_1_2_growType1,
    scope1_2_3Grow_LS = scope_1_2_3_growType5 - scope_1_2_3_growType1,
    s1IntSecDev_LS = s1IntSecDevType5 - s1IntSecDevType1,
    s1_2IntSecDev_LS = s1_2IntSecDevType5 - s1_2IntSecDevType1,
    s1_2_3IntSecDev_LS = s1_2_3IntSecDevType5 - s1_2_3IntSecDevType1
) %>% select(date, ends_with("LS"))

# ts.plot(Portf_LS[,"scope1_LS"])
# mdl1 = lm(scope1_LS ~ 1, data = Portf_LS)
# stargazer(mdl1, type="text")

US_Market <- read_csv("USMARKET_DATEFIX.csv")
PORT_USMARKET <- Portf_LS %>% mutate(
    date = as.Date(date)
) %>%
    left_join(US_Market, by = join_by(date == Mkt_date))

# PORT_USMARKET %>% mutate_if(is.numeric, round, digit = 3)

write.csv(PORT_USMARKET, "PORT_USMARKET.csv")

# line plot of absolute emissions
PORT_USMARKET %>% 
    select(date, scope1_LS:scope1_2_3_LS) %>%
    na.omit() %>% 
    mutate(
    across(scope1_LS:scope1_2_3_LS, ~(as.numeric(.x)+1))
) %>%
    mutate(
    across(scope1_LS:scope1_2_3_LS, cumprod)
) %>%
    pivot_longer(
        cols = ends_with("LS"),
        names_to = "LS_type",
        values_to = "ret"
    ) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = ret, color = LS_type)) +
    labs(x = "Date", y = "Cumulative returns") +
    scale_y_continuous(labels = scales::percent)

# line plot of emission growth
PORT_USMARKET %>% 
    select(date, scope1Grow_LS:scope1_2_3Grow_LS) %>%
    na.omit() %>% 
    mutate(
        across(scope1Grow_LS:scope1_2_3Grow_LS, ~(as.numeric(.x)+1))
    ) %>%
    mutate(
        across(scope1Grow_LS:scope1_2_3Grow_LS, cumprod)
    ) %>%
    pivot_longer(
        cols = ends_with("LS"),
        names_to = "LS_type",
        values_to = "ret"
    ) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = ret, color = LS_type)) +
    labs(x = "Date", y = "Cumulative returns") +
    scale_y_continuous(labels = scales::percent)

# line plot of emission intensity
PORT_USMARKET %>% 
    select(date, contains("Int")) %>%
    na.omit() %>% 
    mutate(
        across(scope1Int_LS:s1_2_3IntSecDev_LS, ~(as.numeric(.x)+1))
    ) %>%
    mutate(
        across(scope1Int_LS:s1_2_3IntSecDev_LS, cumprod)
    ) %>%
    pivot_longer(
        cols = ends_with("LS"),
        names_to = "LS_type",
        values_to = "ret"
    ) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = ret, color = LS_type)) +
    labs(x = "Date", y = "Cumulative returns") +
    scale_y_continuous(labels = scales::percent)