library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(ggplot2)
library(ggthemes)
library(GRS.test)
library(lmtest)
library(sandwich)
library(ggrepel)
library(frenchdata)
library(purrr)
library(broom)

data_sets <- get_french_data_list()

ff_3_factors <- download_french_data("Fama/French 3 Factors")
monthly_ff_3_factors <- ff_3_factors$subsets$data[[1]]
PORT_USMARKET <- read.csv("PORT_USMARKET.csv")

PORT_USMARKET_LS <- PORT_USMARKET %>% 
                    select(date,
                           RF,
                           scope1_LS, 
                           scope2_LS, 
                           scope3_LS,
                           scope1_2_LS,
                           scope1_2_3_LS,
                           scope1Grow_LS,
                           scope1_2Grow_LS,
                           scope1_2_3Grow_LS,
                           scope1Int_LS,
                           scope1_2Int_LS,
                           scope1_2_3Int_LS,
                           s1IntSecDev_LS,
                           s1_2IntSecDev_LS,
                           s1_2_3IntSecDev_LS
                           ) %>%
                    mutate(
                        scope1_LS = scope1_LS * -1, 
                        scope2_LS = scope2_LS * -1, 
                        scope3_LS = scope3_LS * -1,
                        scope1_2_LS = scope1_2_LS * -1,
                        scope1_2_3_LS = scope1_2_3_LS * -1,
                        scope1Grow_LS = scope1Grow_LS * -1,
                        scope1_2Grow_LS = scope1_2Grow_LS * -1,
                        scope1_2_3Grow_LS = scope1_2_3Grow_LS * -1,
                        scope1Int_LS = scope1Int_LS * -1,
                        scope1_2Int_LS = scope1_2Int_LS * -1,
                        scope1_2_3Int_LS = scope1_2_3Int_LS * -1,
                        s1IntSecDev_LS = s1IntSecDev_LS * -1,
                        s1_2IntSecDev_LS = s1_2IntSecDev_LS * -1,
                        s1_2_3IntSecDev_LS = s1_2_3IntSecDev_LS * -1
                    )
PORT_USMARKET_LS_nodate <- PORT_USMARKET_LS %>% select(-date, -RF)

# launch t-test

t_temp <- numeric(ncol(PORT_USMARKET_LS_nodate))

for(i in 1:ncol(PORT_USMARKET_LS_nodate)){
    t_result <- t.test(PORT_USMARKET_LS_nodate[,i], mu = 0)
    t_temp[i] <- t_result$p.value
}

# calculate sharpe ratio

PORT_USMARKET_LS_xts <- xts(PORT_USMARKET_LS[, -1],
                            order.by = as.Date(PORT_USMARKET_LS$date)) 

sharpe_temp <- numeric(ncol(PORT_USMARKET_LS_xts)-1)

for(i in 1:ncol(PORT_USMARKET_LS_xts)-1){
    sharpe_temp[i] <- SharpeRatio.annualized(PORT_USMARKET_LS_xts[, i+1, drop = FALSE], 
                                             Rf = 0,
                                             scale = 12)
}

# calculate sharpe ratio manually

LSmeans <- colMeans(PORT_USMARKET_LS_nodate, na.rm = TRUE)
LSsd <- sapply(PORT_USMARKET_LS_nodate, sd, na.rm = TRUE)
LS_sharpe <- (LSmeans/LSsd) * sqrt(12)


monthly_ff_3_factors <- monthly_ff_3_factors %>% 
                        select(-`Mkt-RF`, -RF) %>% 
                        mutate(
                        date = as.Date(
                            cut(as.Date(
                                paste0(date, "01"), 
                                format = "%Y%m%d") + 32, 'month')) - 1,
                        SMB = SMB / 100,
                        HML = HML / 100
                     )

# join fama's data with long-short portfolio

PORT_USMARKET <- PORT_USMARKET %>% 
    mutate(
    date = ymd(date)
) %>%
    select(
        date,
        Mkt_RF,
        scope1_LS, 
        scope2_LS, 
        scope3_LS,
        scope1_2_LS,
        scope1_2_3_LS,
        scope1Grow_LS,
        scope1_2Grow_LS,
        scope1_2_3Grow_LS,
        scope1Int_LS,
        scope1_2Int_LS,
        scope1_2_3Int_LS,
        s1IntSecDev_LS,
        s1_2IntSecDev_LS,
        s1_2_3IntSecDev_LS
    )

PORT_USMARKET_LS_ff <- PORT_USMARKET %>%
    left_join(monthly_ff_3_factors, by = "date") %>%
    left_join(PORT_USMARKET) %>%
    select(
        date,
        Mkt_RF,
        SMB,
        HML,
        scope1_LS, 
        scope2_LS, 
        scope3_LS,
        scope1_2_LS,
        scope1_2_3_LS,
        scope1Grow_LS,
        scope1_2Grow_LS,
        scope1_2_3Grow_LS,
        scope1Int_LS,
        scope1_2Int_LS,
        scope1_2_3Int_LS,
        s1IntSecDev_LS,
        s1_2IntSecDev_LS,
        s1_2_3IntSecDev_LS
    ) %>%
    mutate(
        scope1_LS = scope1_LS * -1, 
        scope2_LS = scope2_LS * -1, 
        scope3_LS = scope3_LS * -1,
        scope1_2_LS = scope1_2_LS * -1,
        scope1_2_3_LS = scope1_2_3_LS * -1,
        scope1Grow_LS = scope1Grow_LS * -1,
        scope1_2Grow_LS = scope1_2Grow_LS * -1,
        scope1_2_3Grow_LS = scope1_2_3Grow_LS * -1,
        scope1Int_LS = scope1Int_LS * -1,
        scope1_2Int_LS = scope1_2Int_LS * -1,
        scope1_2_3Int_LS = scope1_2_3Int_LS * -1,
        s1IntSecDev_LS = s1IntSecDev_LS * -1,
        s1_2IntSecDev_LS = s1_2IntSecDev_LS * -1,
        s1_2_3IntSecDev_LS = s1_2_3IntSecDev_LS * -1
    )

# regression to 1

mode_LS <- lapply(paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "1", sep = "~"
    ), formula
)
res.mode_LS <- lapply(mode_LS, FUN = function(x){
    summary(lm(formula = x, data = PORT_USMARKET_LS_ff))
    }
)
names(res.mode_LS) <- paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "1", sep = "~"
)
result <- map(res.mode_LS, glance)

# regression to Mkt_RF

mode_LS_Mkt_RF <- lapply(paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "Mkt_RF", sep = "~"
    ), formula
)
res.mode_LS_Mkt_RF <- lapply(mode_LS_Mkt_RF, FUN = function(x){
    summary(lm(formula = x, data = PORT_USMARKET_LS_ff))
    }
)
names(res.mode_LS_Mkt_RF) <- paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "Mkt_RF", sep = "~"
)
result_list_Mkt_RF <- map(res.mode_LS_Mkt_RF, glance)
result_Mkt_RF <- do.call(rbind, result_list_Mkt_RF)

# regression to Mkt_RF, HML, SMB

mode_LS_ff <- lapply(paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "Mkt_RF + HML + SMB", sep = "~"
), formula
)
res.mode_LS_ff <- lapply(mode_LS_ff, FUN = function(x){
    summary(lm(formula = x, data = PORT_USMARKET_LS_ff))
}
)
names(res.mode_LS_ff) <- paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "Mkt_RF + HML + SMB", sep = "~"
)
result_list_ff <- map(res.mode_LS_ff, glance)
result_ff <- do.call(rbind, result_list_ff)

# regression to Mkt_RF broom::tidy method
Mkt_result <- PORT_USMARKET_LS_ff %>%
    select(-date, -SMB, -HML) %>%
    gather(portfolio, return, -Mkt_RF) %>%
    group_by(portfolio) %>%
    do(tidy(lm(return~Mkt_RF, .)))

# regression to Mkt_RF, HML, SMB broom::tidy method

ff_result <- PORT_USMARKET_LS_ff %>%
    select(-date) %>%
    gather(portfolio, return, -c("Mkt_RF", "HML", "SMB")) %>%
    group_by(portfolio) %>%
    do(tidy(lm(return~Mkt_RF + HML + SMB, .)))
