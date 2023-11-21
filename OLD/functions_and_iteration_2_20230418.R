# Functions and Iteration
# Gabriel Odom
# 2023-04-18


library(gapminder) # for Gapminder data

# We want to APPLY (map) a FUNCTION (that we wrote) to EACH ELEMENT of a VECTOR


######  Reviewing Vectors  ####################################################

###  Atomic vs Non-Atomic  ###


###  Vector Elements  ###



######  Reviewing Functions  ##################################################

# Functions have 3 main components:

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



######  Applying Functions  ###################################################

countries_ls <- split(
  x = gapminder,
  f = ~ country
)


###  Easy Example  ###
# Remember to load the Tidyverse (for the purrr package)
library(tidyverse)


###  Medium Example  ###


###  A More Challenging Example  ###

