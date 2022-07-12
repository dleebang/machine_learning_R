# Overall accuracy can sometimes be a deceptive measure because of unbalanced
# classes
# 
# A general improvement to using overall accuracy is to study sensitivity and 
# specificity separately. SENSITIVITY, also known as the TRUE POSITIVE RATE or 
# RECALL, is the proportion of actual positive outcomes correctly identified
# as such. SPECIFICITY, also known as the TRUE NEGATIVE RATE, is the proportion
# of actual negative outcomes that are correctly identified as such
# 
# A confusion matrix tabulates each combination of prediction and actual value
# You can create a confusion matrix in R using the table() function or the
# confusionMatrix() function from the caret package.
# 
# See https://rafalab.github.io/dsbook/introduction-to-machine-learning.html#the-confusion-matrix
# for a table summarizing the definitions of sensitivity and specificity and 
# formulas for proportions/probabilities

## Code
# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex)) #compute proportions of accuracy for each gender

prev <- mean(y == "Male") #prevalence of males is much higher
prev

cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm$overall["Accuracy"]
cm$byClass[c("Sensitivity","Specificity", "Prevalence")]
