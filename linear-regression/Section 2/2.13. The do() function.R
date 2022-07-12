# The do() function serves as a bridge btw R base functions, such as lm(), and
# the tidyverse.
# 
# Remember that the output of a pipe string must be a dataframe so the next function
# in the pipe string can work well with the input
# 
# We have to specify (name) a column when using the do() function, otherwise 
# we will get an error.
# 
# If the data frame being returned has more than one row, the rows will be
# concatenated appropriately. 


## Code
# use do to fit a regression line to each HR stratum
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

# using do without naming a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a concatenated column containing data frames 
# will be returned if you specify a column name
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             estimate = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))