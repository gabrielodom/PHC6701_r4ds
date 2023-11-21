# Intro to purrr
# Gabriel Odom
# 2023-04-10

# We want to APPLY a FUNCTION (that we wrote) to EACH ELEMENT of a VECTOR

# Link to materials:
# https://cougrstats.wordpress.com/2020/02/19/an-introduction-to-working-with-lists-using-purrr/

# Link to zipped data (1.7Mb unzipped):
# https://drive.google.com/drive/folders/1HDeAg0EtqE_T1PuRBjKsczR8_ev3XKIA



library(tidyverse)

# 0. Reviewing Functions --------------------------------------------------

MySummaryFun <- function(x_num) {
  c(
    Min = min(x_num),
    Q1 = quantile(x_num, 0.25),
    Median = median(x_num),
    Mean = mean(x_num),
    Q3 = quantile(x_num, 0.75),
    Max = max(x_num),
    StdDev = sd(x_num),
    IQR = IQR(x_num)
  )
}

# Test
MySummaryFun(0:10)

# Apply (map) Test

test_ls <- list(
  a = 0:9,
  b = 10:19,
  c = 20:29
)

map(
  .x = test_ls,
  .f = MySummaryFun
)



# 1. Reading in data ------------------------------------------------------

# Review: how to import a data set with readr::
tornados08_df <- read_csv(file = "data/2008_torn.csv")

# Create a VECTOR of the names / paths to the data sets
dataPaths_char <- list.files(path = "data", full.names = TRUE)

# APPLY the read_csv() FUNCTION to EACH of the file paths in the VECTOR above
?map

tornadoData_ls <- map(
  .x = dataPaths_char,
  .f = read_csv
)

# Name this list
names(tornadoData_ls) <- dataPaths_char

# Extract the states where tornadoes occurred in each year:
map(
  .x = tornadoData_ls,
  .f = "st"
)

# Extract the *unique* states where tornadoes occured in each year
map(
  .x = tornadoData_ls,
  # .f = function(x) {
  .f = ~ {
    # unique(.x[["st"]])
    .x %>%
      pull("st") %>%
      unique()
  }
)




# 2. Iteratively performing analyses --------------------------------------



# 3. Summarizing and condensing datasets ----------------------------------



# 4. Splitting into lists and extracting list items -----------------------



# 5. Iterating over multiple vectors or lists -----------------------------



