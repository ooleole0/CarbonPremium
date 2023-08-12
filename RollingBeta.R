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

# create a function which checks on if the time series is too short,
# if not, return beta.
estimate_capm <- function(data, min_obs = 1) {
    if (nrow(data) < min_obs) {
        beta <- as.numeric(NA)
    } else {
        fit <- lm(Ret ~ scope1_LS, data = data)
        beta <- as.numeric(coefficients(fit)[2])
    }
    return(beta)
}

roll_capm_estimation <- function(data, months, min_obs) {
    # arrange all rows
    data <- data %>%
        arrange(date)
    # compute betas by sliding across months
    betas <- slide_period_vec(
        .x = data,
        .i = data$date,
        .period = "month",
        .f = ~ estimate_capm(., min_obs),
        .before = months - 1,
        .complete = FALSE
    )
    # return a tibble with months and corresponding beta estimates
    return(tibble(
        date = unique(data$date),
        beta = betas
    ))
}

# take 1101 as the example
beta_example <- tej_port_us %>%
    filter(Ticker == 1101) %>%
    mutate(roll_capm_estimation(pick(everything()), months = 60, min_obs = 48)) %>%
    drop_na() %>%
    select(Ticker, CName, date, Ret, scope1_LS, beta) %>%
    mutate_if(is.numeric, round, digits = 3)

# betas group by Tickers
tej_port_us_nested <- tej_port_us %>%
    nest(data = c(date, Ret, scope1_LS), .by = c(Ticker, BCode, BName))

plan(multisession, workers = availableCores())

beta_nested <- tej_port_us_nested %>%
    mutate(beta = furrr::future_map(
        data,
        ~ roll_capm_estimation(., months = 60, min_obs = 48)
    )) %>%
    unnest(beta) %>%
    select(Ticker, date, beta_monthly = beta) %>%
    drop_na() %>%
    mutate_if(is.numeric, round, digits = 3)

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

# create all TW market beta
tw_beta <- roll_capm_estimation(tej_port_us, 60, 36)

alltw <- lm(beta ~ 1, data = tw_beta)