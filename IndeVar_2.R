library(tidyverse)
library(zoo)
library(lubridate)

IndeVar1 <- read_csv("IndeVar1.csv", na = ".")
# compute carbon growth rate
IndeVar2 <- IndeVar1 %>% group_by(gvkey) %>%
    mutate(
        scope_1_grow = (scope_1 - lag(scope_1)) / lag(scope_1),
        scope_1_2_grow = (scope_1_2 - lag(scope_1_2)) / lag(scope_1_2),
        scope_1_2_3_grow = (scope_1_2_3 - lag(scope_1_2_3)) / lag(scope_1_2_3),
    ) %>%
    ungroup()

write.csv(IndeVar2, file = "IndeVar2.csv")