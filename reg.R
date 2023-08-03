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

data_sets <- get_french_data_list()

ff_3_factors <- download_french_data("Fama/French 3 Factors")
monthly_ff_3_factors <- ff_3_factors$subsets$data[[1]]
PORT_USMARKET <- read.csv("PORT_USMARKET.csv")

PORT_USMARKET_LS <- PORT_USMARKET %>% 
                    select(scope1LS, 
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

# launch t-test

t_temp <- numeric(ncol(PORT_USMARKET_LS))

for(i in 1:ncol(PORT_USMARKET_LS)){
    t_result <- t.test(PORT_USMARKET[,"Mkt"], PORT_USMARKET_LS[,i])
    t_temp[i] <- t_result$p.value
}



monthly_ff_3_factors <- monthly_ff_3_factors %>% 
                        select(-`Mkt-RF`, -RF) %>% 
                        mutate(
                        date = as.Date(cut(as.Date(paste0(date, "01"), format = "%Y%m%d") + 32, 'month')) - 1,
                        SMB = SMB / 100,
                        HML = HML /100
                     )

# start here

dat = merge(PORT_USMARKET,monthly_ff_3_factors) 
dat = dat[dat$date>196306,]

Y = dat[,2:26]
x = as.matrix(dat[,c("Mkt-RF","SMB","HML")])
mx = colMeans(x)
rf = dat[,"RF"]
Act_Ret = numeric(ncol(Y))
Pred_Ret = Act_Ret

for(i in 1:ncol(Y)){
    y = Y[,i]-rf
    #  mdl = lm(y ~ x[,"Mkt-RF"])
    #  Pred_Ret[i] = mdl$coef[2]*mx[1]
    mdl = lm(y ~ x) 
    Pred_Ret[i] = sum(mdl$coef[2:4]*mx)
    Act_Ret[i] = mean(y)
}

# valuate alpha and beta
ret_mat <- PORT_USMARKET[, 5:88] - PORT_USMARKET$RF
Mkt_RF_mat <- PORT_USMARKET$Mkt - PORT_USMARKET$RF
GRS_result <- GRS.test(ret_mat, Mkt_RF_mat)

# regression by groups
sd_m_RF <- PORT_USMARKET$scope1Type_1 - PORT_USMARKET$RF
Mkt_m_RF <- PORT_USMARKET$Mkt - PORT_USMARKET$RF
capm_fit <- lm(sd_m_RF ~ Mkt_m_RF, PORT_USMARKET)
coeftest(capm_fit, vcov = NeweyWest)