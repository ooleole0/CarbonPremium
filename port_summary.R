port_mean <- numeric(ncol(PORT_USMARKET_LS_ff)-4)
port_sd <- port_mean
port_skewness <- port_mean
port_kurtosis <- port_mean


for (i in 5:ncol(PORT_USMARKET_LS_ff)){
    port_mean[i-4] <- mean(PORT_USMARKET_LS_ff[,i], na.rm = TRUE)
    port_sd[i-4] <- sd(PORT_USMARKET_LS_ff[,i], na.rm = TRUE)
    port_skewness[i-4] <- skewness(PORT_USMARKET_LS_ff[,i])
    port_kurtosis[i-4] <- kurtosis(PORT_USMARKET_LS_ff[,i])
}