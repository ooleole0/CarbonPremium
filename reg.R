library(tidyverse)
library(zoo)
library(lubridate)
library(GRS.test)

PORT_USMARKET <- read.csv("PORT_USMARKET.csv", na = ".")

Y = PORT_USMARKET[,2:50]
x = as.matrix(PORT_USMARKET[,"Mkt_RF"])
mx = colMeans(x)
rf = PORT_USMARKET[,"RF"]
Act_Ret = numeric(ncol(Y))
Pred_Ret = Act_Ret

for(i in 1:ncol(Y)){
    y = Y[,i]-rf
    #  mdl = lm(y ~ x[,"Mkt-RF"])
    #  Pred_Ret[i] = mdl$coef[2]*mx[1]
    mdl = lm(y ~ x) 
    Pred_Ret[i] = sum(mdl$coef[2]*mx)
    Act_Ret[i] = mean(y)
}