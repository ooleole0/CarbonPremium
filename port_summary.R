library(stargazer)
library(PerformanceAnalytics)

PORT_USMARKET_LS_ff <- read.csv("PORT_USMARKET_LS_ff.csv")
port_mean <- numeric(ncol(PORT_USMARKET_LS_ff)-5)
port_sd <- port_mean
port_skewness <- port_mean
port_kurtosis <- port_mean
port_names <- colnames(PORT_USMARKET_LS_ff[, 6:19])

# summary for us carbon LS portfolios
for (i in 6:ncol(PORT_USMARKET_LS_ff)){
    port_mean[i-5] <- mean(PORT_USMARKET_LS_ff[,i], na.rm = TRUE)
    port_sd[i-5] <- sd(PORT_USMARKET_LS_ff[,i], na.rm = TRUE)
    port_skewness[i-5] <- skewness(PORT_USMARKET_LS_ff[,i], na.rm = TRUE)
    port_kurtosis[i-5] <- kurtosis(PORT_USMARKET_LS_ff[,i], na.rm = TRUE)
}
# calculate sharpe ratio manually
port_sharpe <- (port_mean/port_sd) * sqrt(12)

port_name <- "Long-short Portfolios"
mean_name <- "Mean"
sd_name <- "St. Dev."
sharpe_name <- "Sharpe Ratio"
skew_name <- "Skewness"
kurt_name <- "Kurtosis"

df <- data.frame(
    port_names,
    port_mean,
    port_sd,
    port_skewness,
    port_kurtosis,
    port_sharpe
)
names(df) <- c(port_name, mean_name, sd_name, skew_name, kurt_name, sharpe_name)
df <- mutate_if(df, is.numeric, round, digits = 3)

stargazer(df, type = "html", out = "df.doc", summary = FALSE)

# summary for tw carbon beta LS protfolios
beta_df <- read.csv("TWportLS.csv") %>%
    select(scope1_LS:s1_2_3IntSecDev_LS)

beta_mean <- numeric(ncol(beta_df))
beta_sd <- beta_mean
beta_skewness <- beta_mean
beta_kurtosis <- beta_mean
beta_names <- colnames(beta_df)

for (i in 1:ncol(beta_df)){
    beta_mean[i] <- mean(beta_df[,i], na.rm = TRUE)
    beta_sd[i] <- sd(beta_df[,i], na.rm = TRUE)
    beta_skewness[i] <- skewness(beta_df[,i], na.rm = TRUE)
    beta_kurtosis[i] <- kurtosis(beta_df[,i], na.rm = TRUE)
}
beta_sharpe <- (beta_mean/beta_sd) * sqrt(12)

port_name <- "Long-short Portfolios"
mean_name <- "Mean"
sd_name <- "St. Dev."
sharpe_name <- "Sharpe Ratio"
skew_name <- "Skewness"
kurt_name <- "Kurtosis"

df2 <- data.frame(
    beta_names,
    beta_mean,
    beta_sd,
    beta_skewness,
    beta_kurtosis,
    beta_sharpe
)

names(df2) <- c(port_name, mean_name, sd_name, skew_name, kurt_name, sharpe_name)
df2 <- mutate_if(df2, is.numeric, round, digits = 3)

stargazer(df2, type = "html", out = "df2.doc", summary = FALSE)
