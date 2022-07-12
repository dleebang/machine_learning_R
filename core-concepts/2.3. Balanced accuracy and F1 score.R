# For optmization purposes, sometimes it is more useful to have a one number
# summary than studying both specificity and sensitivity. One preferred metric
# is the BALANCED ACCURACY. Because specificity and sensitivity are rates, it is
# more appropriate to compute the HARMONIC average. In fact, the F1-score, a widely
# used one-number summary, is the harmonic average of precision and recall:
# 
#  The formula: 1 / ( 1/2 * (1 / recall + 1 / precision) )
#  
#  Or written like this: 2 * (precision * recall / precision + recall)
# 
# 
# The F1-score can be adapted to weight specificity and sensitivity differently
# To do this, we define beta to represent how much more important is sensitivity
# compared to specificity and consider a weighted harmonic average:
# 
#  The formula: 1 / ( (beta^2/1+beta^2) * (1/recall) ) + ( (1/1+beta^2) * (1/precision) )
# 
# You can compute the F1-score using the F_meas() function in the caret
# package. The default for beta in the function is 1. 
# 
# 
## Code
# calculate F-1 score for each cutoff
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)  # the F_1 score is maximized at 0.614

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))

sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)
