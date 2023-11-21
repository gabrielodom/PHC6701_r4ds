# Class Notes for PHC6099
# Gabriel Odom
# 2019-11-07
# Last edited: 2019-11-07 (GJO)

# These are basic notes over the tidyverse dplyr package. We have already
#   reviewed the pipe operator. The example we covered was to restructure
#   some complicated code via the pipe.

library(tidyverse)
library(nycflights13)
x <- arrange(mutate(summarise(group_by(flights, dest), delay = mean(arr_delay, na.rm = TRUE)), resid_delay = delay - mean(delay, na.rm = TRUE)), resid_delay)

flights %>% 
  group_by(dest) %>% 
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>% 
  mutate(resid_delay = delay - mean(delay, na.rm = TRUE)) %>% 
  arrange(resid_delay)


flights %>%
  # Find the snowbirds (Miami in the winter)
  filter(dest == "MIA", month %in% c(11, 12, 1, 2)) %>%
  # Keep the delay metrics and the brand
  select(dep_delay, arr_delay, carrier) %>%
  # Who has the least delay?
  arrange(desc(arr_delay)) %>% 
  # Find delay in hours
  mutate(arr_delay_H = arr_delay / 60)


