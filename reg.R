library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(ggplot2)
library(ggthemes)
library(GRS.test)
library(lmtest)
library(sandwich)
library(ggrepel)

PORT_USMARKET <- read.csv("PORT_USMARKET.csv")

# valuate alpha and beta
ret_mat <- PORT_USMARKET[, 5:74] - PORT_USMARKET$RF
Mkt_RF_mat <- PORT_USMARKET$Mkt - PORT_USMARKET$RF
GRS_result <- GRS.test(ret_mat, Mkt_RF_mat)

# regression by groups
sd_m_RF <- PORT_USMARKET$scope1Type_1 - PORT_USMARKET$RF
Mkt_m_RF <- PORT_USMARKET$Mkt - PORT_USMARKET$RF
capm_fit <- lm(sd_m_RF ~ Mkt_m_RF, PORT_USMARKET)
coeftest(capm_fit, vcov = NeweyWest)