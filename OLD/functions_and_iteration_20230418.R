# Functions and Iteration
# Gabriel Odom
# 2023-04-18


library(gapminder) # for Gapminder data
library(tidyverse)

# We want to APPLY (map) a FUNCTION (that we wrote) to EACH ELEMENT of a VECTOR


######  Review  ###############################################################

###  Vectors  ###
# Vectors are collection of objects (often stored within an object)
x <- c(1, 3, 5)  # atomic vector
# non-atomic vector (list)
me_ls <- list(
  forname = "Gabriel",
  surname = "Odom",
  age = 35
)
head(gapminder)


###  Vector Elements  ###
x[2]
me_ls$forname




######  Reviewing Functions  ##################################################

# Functions have 3 main components:
#   - Name: object in the environment (Global Environment for example) that
#       "holds" the code we write
#   - Arguments: inputs of the function; what does the user need to provide in
#       order for the function to work
#   - Body: what the function does; that is, the code that we write

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
countries_ls$Cameroon
nrow(countries_ls$Cameroon)
ncol(countries_ls$Cameroon)
dim(countries_ls$Cameroon)


###  Easy Example  ###
# Remember to load the Tidyverse (for the purrr package)
map(
  .x = countries_ls,
  .f = dim
)

###  Medium Example  ###
map(
  .x = countries_ls,
  .f = "lifeExp"
)

###  A More Challenging Example  ###
map(
  .x = countries_ls,
  .f = "lifeExp"
) %>%
  map(.f = MySummaryFun)
