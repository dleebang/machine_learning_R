# The predict() function takes a fitted object from functions such as lm()
# or glm() and a data frame with the new predictors for which to predict. 
# 
# predict() is a generic function in R that calls other functions depending on
# what kind of object it receives. To learn about specifics, you can read
# the help files: ?predict.lm or ?predict.glm
# 

## Code
# fit and test_set defined in the previous script
#using predict is the same as applying the regression formula 
y_hat <- predict(fit, test_set) 

#as such we get the same squared loss
mean((y_hat - test_set$son)^2) 



