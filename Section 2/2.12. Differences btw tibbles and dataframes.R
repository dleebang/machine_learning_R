# Tibbles are more readable than data frames
# 
# If you subset a data frame, you may not get a data frame. If you subset
# a tibble, you always get a tibble. 
# 
# Tibbles can hold more complex objects such as lists or functions. DFs can
# only hold original vectors of numbers or boolean. 
# 
# Tibbles can be grouped and functions like group_by() recognize such groupings
# 

## Code
# inspect data frame and tibble
Teams
as_tibble(Teams)
# Note that the function was formerly called as.tibble()

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as_tibble(Teams[,20]))

# pulling a vector out of a tibble - use dollar sign
class(as_tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as_tibble(Teams)$hr

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))
