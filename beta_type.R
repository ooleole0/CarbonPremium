library(tidyverse)
library(stargazer)
library(broom)

beta_df <- read_csv("betas.csv") %>%
    select(-...1)

beta_tiled <- beta_df %>%
    group_by(date) %>%
    mutate(
        across(2:15, ~ntile(., 5))
    ) %>% 
    group_by(Ticker) %>%
    mutate(sortdate = lag(date)) %>%
    ungroup()

TEJ <- read_csv("TEJ_CLEAN.csv")

TEJ_beta <- TEJ %>%
    group_by(Ticker) %>%
    mutate(weight = lag(MktSize)) %>%
    right_join(beta_tiled, by = join_by(Ticker, date == sortdate)) %>%
    select(-date.y)

wide <- function(df, var, var_name){
    output <- df %>%
        drop_na({{var}}) %>%
        group_by(date, {{var}}) %>%
        summarise(vwret = weighted.mean(Ret,w=weight,na.rm=T)) %>%
        ungroup() %>%
        pivot_wider(
            id_cols = date,
            names_from = {{var}},
            values_from = vwret,
            names_prefix = paste0(var_name, "Type")
        )
    return(as_tibble(output))
}

Portf_scope_1 <- wide(TEJ_beta, scope1_LS_beta, "scope1_LS_beta")
Portf_scope_2 <- wide(TEJ_beta, scope2_LS_beta, "scope2_LS_beta")
Portf_scope_3 <- wide(TEJ_beta, scope3_LS_beta, "scope3_LS_beta")
Portf_scope_1_2 <- wide(TEJ_beta, scope1_2_LS_beta, "scope1_2_LS_beta")
Portf_scope_1_2_3 <- wide(TEJ_beta, scope1_2_3_LS_beta, "scope1_2_3_LS_beta")
Portf_scope_1_Int <- wide(TEJ_beta, scope1Int_LS_beta, "scope1Int_LS_beta")
Portf_scope_1_2_Int <- wide(TEJ_beta, scope1_2Int_LS_beta, "scope1_2Int_LS_beta")
Portf_scope_1_2_3_Int <- wide(TEJ_beta, scope1_2_3Int_LS_beta, "scope1_2_3Int_LS_beta")
Portf_scope_1_grow <- wide(TEJ_beta, scope1Grow_LS_beta, "scope1Grow_LS_beta")
Portf_scope_1_2_grow <- wide(TEJ_beta, scope1_2Grow_LS_beta, "scope1_2Grow_LS_beta")
Portf_scope_1_2_3_grow <- wide(TEJ_beta, scope1_2_3Grow_LS_beta, "scope1_2_3Grow_LS_beta")
Portf_s1IntSec <- wide(TEJ_beta, s1IntSecDev_LS_beta, "s1IntSecDev_LS_beta")
Portf_s1_2IntSec <- wide(TEJ_beta, s1_2IntSecDev_LS_beta, "s1_2IntSecDev_LS_beta")
Portf_s1_2_3IntSec <- wide(TEJ_beta, s1_2_3IntSecDev_LS_beta, "s1_2_3IntSecDev_LS_beta")

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
    Portf_s1IntSec,
    Portf_s1_2IntSec,
    Portf_s1_2_3IntSec
)
Portf <- Portf_list %>% reduce(full_join, by = "date")

Portf_LS <- Portf %>% 
    mutate(
        scope1_LS = scope1_LS_betaType5 - scope1_LS_betaType1,
        scope2_LS = scope2_LS_betaType5 - scope2_LS_betaType1,
        scope3_LS = scope3_LS_betaType5 - scope3_LS_betaType1,
        scope1_2_LS = scope1_2_LS_betaType5 - scope1_2_LS_betaType1,
        scope1_2_3_LS = scope1_2_3_LS_betaType5 - scope1_2_3_LS_betaType1,
        scope1Grow_LS = scope1Grow_LS_betaType5 - scope1Grow_LS_betaType1,
        scope1_2Grow_LS = scope1_2Grow_LS_betaType5 - scope1_2Grow_LS_betaType1,
        scope1_2_3Grow_LS = scope1_2_3Grow_LS_betaType5 - scope1_2_3Grow_LS_betaType1,
        scope1Int_LS = scope1Int_LS_betaType5 - scope1Int_LS_betaType1,
        scope1_2Int_LS = scope1_2Int_LS_betaType5 - scope1_2Int_LS_betaType1,
        scope1_2_3Int_LS = scope1_2_3Int_LS_betaType5 - scope1_2_3Int_LS_betaType1,
        s1IntSecDev_LS = s1IntSecDev_LS_betaType5 - s1IntSecDev_LS_betaType1,
        s1_2IntSecDev_LS = s1_2IntSecDev_LS_betaType5 - s1_2IntSecDev_LS_betaType1,
        s1_2_3IntSecDev_LS = s1_2_3IntSecDev_LS_betaType5 - s1_2_3IntSecDev_LS_betaType1
    ) %>% select(date, ends_with("LS"))

write.csv(Portf_LS, "TWportLS.csv")

TW_Mkt_Rf <- read_csv("TWMKTRF.csv")

Port_LS_TW <- Portf_LS %>%
    left_join(TW_Mkt_Rf, by = join_by(date == date))

summary(lm(scope1_LS ~ 1, data = Port_LS_TW))

# regression to 1
result_LS_1 <- Port_LS_TW %>%
    select(-date, -TW_Mkt, -TW_Rf, -TW_Mkt_Rf) %>%
    gather(portfolio, return) %>%
    group_by(portfolio) %>%
    do(tidy(lm(return~1, .))) %>%
    ungroup() %>%
    mutate_if(is.numeric, round, digits = 3)
stargazer(result_LS_1, type = "html", out = "TWregResult1.doc", summary = FALSE)

# regression to TW_Mkt_Rf
Mkt_result <- Port_LS_TW %>%
    select(-date, -TW_Mkt, -TW_Rf) %>%
    gather(portfolio, return, -TW_Mkt_Rf) %>%
    group_by(portfolio) %>%
    do(tidy(lm(return~TW_Mkt_Rf, .))) %>%
    ungroup() %>%
    mutate_if(is.numeric, round, digits = 3)

stargazer(Mkt_result, type = "html", out = "TWMkt_result.doc", summary = FALSE)

# create cumulative return dataframe
Portf_LS_cum <- Portf_LS %>% 
    mutate(
        across(ends_with("LS"), ~(as.numeric(.x)+1))
    ) %>%
    mutate(
        across(ends_with("LS"), cumprod)
    ) 
# line plot of absolute emissions
Portf_LS_cum %>% select(date, scope1_LS:scope1_2_3_LS) %>%
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
Portf_LS_cum %>% select(date, scope1Grow_LS:scope1_2_3Grow_LS) %>%
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
Portf_LS_cum %>% select(date, scope1Int_LS:s1_2_3IntSecDev_LS) %>%
    pivot_longer(
        cols = ends_with("LS"),
        names_to = "LS_type",
        values_to = "ret"
    ) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = ret, color = LS_type)) +
    labs(x = "Date", y = "Cumulative returns") +
    scale_y_continuous(labels = scales::percent)

stargazer(Mkt_result, type = "html", out = "TWMkt_result.doc", summary = FALSE)

