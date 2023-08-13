library(tidyverse)
library(slider)
library(future)
library(ggplot2)
library(stargazer)

tej <- read_csv("TEJ_CLEAN.csv")
TWBCode <- read_csv("TWBCode.csv")

tej <- tej %>%
    select(-BName) %>%
    left_join(TWBCode, by = join_by(BCode == Mid_BCode)) %>%
    select(-BCode, -Mid_BName) %>%
    mutate(BCode = Large_BCode, BName = Large_BName) %>%
    select(-Large_BCode)

port_us <- read_csv("PORT_USMARKET_LS_ff.csv") %>%
    select(-...1)

tej_port_us <- tej %>%
    right_join(port_us, by = join_by(date == date))

# create formula lists and names
mode_tej_LS <- lapply(
    paste(
    "Ret", names(tej_port_us)[13:ncol(tej_port_us)], sep = "~"
    ), formula
)
names(mode_tej_LS) <- names(tej_port_us)[13:ncol(tej_port_us)]

# create a function which checks on if the time series is too short,
# if not, return beta.

estimate_capm <- function(data, min_obs = 1, var_port) {
    if (nrow(data) < min_obs) {
        beta <- as.numeric(NA)
    } else {
        fit <- lm(formula = mode_tej_LS[[var_port]], data = data)
        beta <- as.numeric(coefficients(fit)[2])
    }
    return(beta)
}

roll_capm_estimation <- function(data, var_port) {
    # arrange all rows
    data <- data %>%
        arrange(date)
    # compute betas by sliding across months
    betas <- slide_period_vec(
        .x = data,
        .i = data$date,
        .period = "month",
        # minimum observation as 48
        .f = ~ estimate_capm(., min_obs = 48, var_port),
        # backtest period as 60(59+1)
        .before = 59,
        .complete = FALSE
    )
    # return a tibble with months and corresponding beta estimates
    return(tibble(
        date = unique(data$date),
        beta = betas
    ))
}

# use multi-cores for speed
plan(multisession, workers = availableCores())

# estimate betas group by Tickers
# pass data and the "name" of the portfolio you want to reg to in the function
estimate_beta <- function(data, var_port){
    # create nested data
    data_nested <- data %>%
        nest(data = c(date, Ret, {{var_port}}), .by = c(Ticker, BCode, BName))
    
    data_nested %>%
        mutate(
            beta = furrr::future_map(
                data,
                ~ roll_capm_estimation(., var_port)
            )
        ) %>%
        unnest(beta) %>%
        select(
            Ticker, 
            date,
            beta
        ) %>%
        rename_with(
            ~ paste({{var_port}}, "_beta", sep = ""),
            beta
        ) %>%
        drop_na()
}



# beta distribution boxplot by industry

tej_port_us %>%
    left_join(beta_nested, by = c("Ticker", "date")) %>%
    drop_na(beta_monthly) %>%
    group_by(BName, Ticker) %>%
    summarize(beta = mean(beta_monthly), 
              .groups = "drop") %>%
    ggplot(aes(x = reorder(BName, beta, FUN = median), y = beta)) +
    geom_boxplot() +
    coord_flip() +
    labs(
        x = NULL, y = NULL,
        title = "Firm-specific beta distributions by industry"
    )

beta_nested %>%
    drop_na(beta_monthly) %>%
    group_by(date) %>%
    reframe(
        x = quantile(beta_monthly, seq(0.1, 0.9, 0.1)),
        quantile = 100 * seq(0.1, 0.9, 0.1)
    ) %>%
    ggplot(aes(
        x = date, 
        y = x, 
        color = as_factor(quantile),
        linetype = as_factor(quantile)
    )) +
    geom_line() +
    labs(
        x = NULL, y = NULL, color = NULL, linetype = NULL,
        title = "Monthly deciles of estimated betas",
    )