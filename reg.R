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
                           scope1LS, 
                           scope2LS, 
                           scope3LS,
                           scope1_2LS,
                           scope1_2_3LS,
                           scope1GrowLS,
                           scope1_2GrowLS,
                           scope1_2_3GrowLS,
                           scope1IntLS,
                           scope1_2IntLS,
                           scope1_2_3IntLS,
                           s1IntSecDevLS,
                           s1_2IntSecDevLS,
                           s1_2_3IntSecDevLS
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
        scope1LS, 
        scope2LS, 
        scope3LS,
        scope1_2LS,
        scope1_2_3LS,
        scope1GrowLS,
        scope1_2GrowLS,
        scope1_2_3GrowLS,
        scope1IntLS,
        scope1_2IntLS,
        scope1_2_3IntLS,
        s1IntSecDevLS,
        s1_2IntSecDevLS,
        s1_2_3IntSecDevLS
    )

PORT_USMARKET_LS_ff <- PORT_USMARKET %>%
    left_join(monthly_ff_3_factors, by = "date") %>%
    left_join(PORT_USMARKET) %>%
    select(
        date,
        Mkt_RF,
        SMB,
        HML,
        scope1LS, 
        scope2LS, 
        scope3LS,
        scope1_2LS,
        scope1_2_3LS,
        scope1GrowLS,
        scope1_2GrowLS,
        scope1_2_3GrowLS,
        scope1IntLS,
        scope1_2IntLS,
        scope1_2_3IntLS,
        s1IntSecDevLS,
        s1_2IntSecDevLS,
        s1_2_3IntSecDevLS
    )

# regression to 1

models <- lapply(paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "1", sep = "~"
    ), formula
)
res.models <- lapply(models, FUN = function(x){
    summary(lm(formula = x, data = PORT_USMARKET_LS_ff))
    }
)
names(res.models) <- paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "1", sep = "~"
)
result <- map(res.models, glance)

# regression to Mkt_RF

models_Mkt_RF <- lapply(paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "Mkt_RF", sep = "~"
    ), formula
)
res.models_Mkt_RF <- lapply(models_Mkt_RF, FUN = function(x){
    summary(lm(formula = x, data = PORT_USMARKET_LS_ff))
    }
)
names(res.models_Mkt_RF) <- paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "Mkt_RF", sep = "~"
)
result_list_Mkt_RF <- map(res.models_Mkt_RF, glance)
result_Mkt_RF <- do.call(rbind, result_list_Mkt_RF)

# regression to Mkt_RF, HML, SMB

models_ff <- lapply(paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "Mkt_RF + HML + SMB", sep = "~"
), formula
)
res.models_ff <- lapply(models_ff, FUN = function(x){
    summary(lm(formula = x, data = PORT_USMARKET_LS_ff))
}
)
names(res.models_ff) <- paste(
    names(PORT_USMARKET_LS_ff)[5:ncol(PORT_USMARKET_LS_ff)], "Mkt_RF + HML + SMB", sep = "~"
)
result_list_ff <- map(res.models_ff, glance)
result_ff <- do.call(rbind, result_list_ff)
