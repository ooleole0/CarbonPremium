port_mean <- numeric(ncol(PORT_USMARKET_LS_ff)-4)
port_sd <- port_mean
port_skewness <- port_mean
port_kurtosis <- port_mean
port_names <- colnames(PORT_USMARKET_LS_ff[, 5:18])

for (i in 5:ncol(PORT_USMARKET_LS_ff)){
    port_mean[i-4] <- mean(PORT_USMARKET_LS_ff[,i], na.rm = TRUE)
    port_sd[i-4] <- sd(PORT_USMARKET_LS_ff[,i], na.rm = TRUE)
    port_skewness[i-4] <- skewness(PORT_USMARKET_LS_ff[,i])
    port_kurtosis[i-4] <- kurtosis(PORT_USMARKET_LS_ff[,i])
}
# calculate sharpe ratio manually
port_sharpe <- (port_mean/port_sd) * sqrt(12)

port_name <- "Long-short Portfolios"
mean_name <- "Mean"
sd_name <- "Standard Deviation"
sharpe_name <- "Sharpe Ratio"
skew_name <- "Skewness"
kurt_name <- "Kurtosis"

df <- data.frame(
    port_names,
    port_mean,
    port_sd,
    port_sharpe,
    port_skewness,
    port_kurtosis
)
names(df) <- c(port_name, mean_name, sd_name, sharpe_name, skew_name, kurt_name)