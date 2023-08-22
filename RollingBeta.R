library(tidyverse)
library(slider)
library(future)
library(ggplot2)

tej <- read_csv("TEJ_CLEAN.csv")
TWBCode <- read_csv("TWBCode.csv")

tej <- tej %>%
    select(-BName) %>%
    left_join(TWBCode, by = join_by(BCode == Mid_BCode)) %>%
    select(-BCode, -Mid_BName)%>%
    select(date,
           Ticker,
           CName,
           UNINO,
           MktSize,
           Ret,
           BCode = Large_BCode, 
           BName = Large_BName
    )

port_us <- read_csv("PORT_USMARKET_LS_ff.csv") %>%
    select(-...1)

tej_port_us <- tej %>%
    right_join(port_us, by = join_by(date == date))

# create formula lists and names
mode_tej_LS <- lapply(
    paste(
    "Ret", names(tej_port_us)[12:ncol(tej_port_us)], sep = "~"
    ), formula
)
names(mode_tej_LS) <- names(tej_port_us)[12:ncol(tej_port_us)]

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
        .f = ~ estimate_capm(., min_obs = 36, var_port),
        # backtest period as 60(59+1)
        .before = 36,
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
    
    output <- data_nested %>%
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
    
    return(as_tibble(output))
}

# estimate all portfoios
model_name <- names(mode_tej_LS)
beta_list <- vector("list", length(mode_tej_LS))
# don't know why for loop won't work,
# had to manually loop over to create beta list
beta_list[[1]] <- estimate_beta(tej_port_us, model_name[1])
beta_list[[2]] <- estimate_beta(tej_port_us, model_name[2])
beta_list[[3]] <- estimate_beta(tej_port_us, model_name[3])
beta_list[[4]] <- estimate_beta(tej_port_us, model_name[4])
beta_list[[5]] <- estimate_beta(tej_port_us, model_name[5])
beta_list[[6]] <- estimate_beta(tej_port_us, model_name[6])
beta_list[[7]] <- estimate_beta(tej_port_us, model_name[7])
beta_list[[8]] <- estimate_beta(tej_port_us, model_name[8])
beta_list[[9]] <- estimate_beta(tej_port_us, model_name[9])
beta_list[[10]] <- estimate_beta(tej_port_us, model_name[10])
beta_list[[11]] <- estimate_beta(tej_port_us, model_name[11])
beta_list[[12]] <- estimate_beta(tej_port_us, model_name[12])
beta_list[[13]] <- estimate_beta(tej_port_us, model_name[13])
beta_list[[14]] <- estimate_beta(tej_port_us, model_name[14])

# join all betas
beta_df <- beta_list %>%
    reduce(left_join, by = c("Ticker", "date"))
# remove beta list and output beta dataframes to save memory
rm(beta_list)
write.csv(beta_df, "betas.csv")

# beta distribution boxplot by industry
# create dataframe for ploting
df_boxplot <- tej_port_us %>%
    left_join(beta_df, by = c("Ticker", "date")) %>%
    drop_na() %>%
    group_by(BName, Ticker) %>%
    summarise(
        across(scope1_LS_beta:s1_2_3IntSecDev_LS_beta, 
        ~ mean(.x, na.rm = TRUE)
        )
    )
# boxplots for absolute emissions
df_boxplot %>% 
    select(BName, Ticker, scope1_LS_beta: scope1_2_3_LS_beta) %>%
    pivot_longer(
        cols = ends_with("beta"), 
        names_to = "LS_type",
        values_to = "beta"
    ) %>%
    ggplot(aes(x = reorder(BName, beta, FUN = median), y = beta)) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~ LS_type) +
    labs(
        x = NULL, y = NULL,
    )
# boxplots for emissions growths
df_boxplot %>% 
    select(BName, Ticker, scope1Grow_LS_beta: scope1_2_3Grow_LS_beta) %>%
    pivot_longer(
        cols = ends_with("beta"), 
        names_to = "LS_type",
        values_to = "beta"
    ) %>%
    ggplot(aes(x = reorder(BName, beta, FUN = median), y = beta)) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~ LS_type) +
    labs(
        x = NULL, y = NULL,
    )
# boxplots for emission intensity related variables
df_boxplot %>% 
    select(BName, Ticker, scope1Int_LS_beta: s1_2_3IntSecDev_LS_beta) %>%
    pivot_longer(
        cols = ends_with("beta"), 
        names_to = "LS_type",
        values_to = "beta"
    ) %>%
    ggplot(aes(x = reorder(BName, beta, FUN = median), y = beta)) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~ LS_type) +
    labs(
        x = NULL, y = NULL,
    )